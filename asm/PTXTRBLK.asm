*          DATA SET PTXTRBLK   AT LEVEL 108 AS OF 06/27/19                      
*PHASE QXTRBLKC                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PPBYOUT                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PUBEDIT                                                                
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE PPGETCON                                                               
*INCLUDE PPGETCU                                                                
*INCLUDE PTXBROTS                 XTRACT RECORD CREATION MODULE                 
*INCLUDE PTXBCNVX                 CONVERSION ROUTINES FOR ALL ABOVE             
         TITLE 'PTXTRBLK - EXTRACT PRINT SYSTEM FILE SQL DATA'                  
**********************************************************************          
*  ID  LVL   DATE    TICKET            COMMENTS                      *          
* ---- --- ------- ------------ -------------------------------------*          
* JSAY 001 13DEC18 <SPEC-30370> BLOCKCHAIN PRINT EXTRACT             *          
* GHOA 003 09JAN19              SUPPORT MULTIPLE INVOICES X'51' ELEM *          
* GHOA 004 11JAN19              MORE WORK FOR MULTIPLE INVOICES      *          
* GHOA 103 15FEB19              GENERATE IF ONLY BUY AMT CHANGES     *          
* GHOA 104 15FEB19              FIXED ZERO DOLLAR INVOICE BUG        *          
* GHOA 105 21MAR19              KEEP ADBUYER IF CAMPAIGN ENABLED     *          
*      105 04APR19              MORE ZERO DOLLAR RULES               *          
* JSAY 106 20MAY19              GROSS AMOUNT ISSUE                   *          
* JSAY 107 21MAY19              SKIPPED PRINT UPLOAD RECORDS         *          
* JSAY 108 22APR19 SPEC-34994   EXTRACT CHANGE TO MEDIA AUTHORIZATION*          
*                               NUMBER                               *          
* JSAY 108 17MAY19 SPEC-35854   ELIMINATE COPY/CHANGE PAIR WITHOUT   *          
*                               A COPY RECORD.                       *          
**********************************************************************          
***********************************************************************         
*                                                                     *         
*  PRINT SQL SUB SYSTEM EXTRACT CONTROL MODULE                        *         
*                                                                     *         
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:                     *         
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)               *         
*                                                                     *         
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:               *         
*   DXOPENQ  - OPEN SYSTEM FILES                                      *         
*   DXCLOSEQ - CLOSE SYSTEM FILES                                     *         
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE                        *         
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE                      *         
*                                                                     *         
* FOR DXLOADQ AND DXUPDTQ MODES,                                      *         
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                                 *         
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE - SEE DSECT      *         
*                                                      SXDTABD)       *         
*                                                                     *         
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK               *         
*                                                                     *         
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM, SEE RXUSERD              *         
***********************************************************************         
         EJECT                                                                  
QXTRBLK  CSECT                                                                  
         ENTRY COMFACS                                                          
         ENTRY MASTC                                                            
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 WORKL,*PTXTR**,R9                                                
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
*                                                                               
* MEDTABD,R8 USING IS PROGRAM-WIDE, DON'T USE R8 FOR ANYTHING ELSE!!            
         USING MEDTABD,R8                                                       
*                                                                               
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
DXU      USING RXUSERD,DXUSER                                                   
*                                                                               
         MVC   USRPRM,DXUSER       SET RECORD TYPE(RCV/PAY)                     
*                                                                               
         MVC   SYSCODE,DXSYSCOD                                                 
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
*                                                                               
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
         MVC   PLATFORM,SXDTPLFM                                                
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
LOW      SR    RC,RC                                                            
         CHI   RC,256                                                           
         J     EXIT                                                             
*                                                                               
HIGH     CHI   RC,0                                                             
         J     EXIT                                                             
*                                                                               
EQXIT    DS    0X                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NEQXIT   DS    0X                                                               
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
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
GENINIT  DS    0H                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE BELOW WILL CHECK TO SEE IF THE CCC (CUSTOM COLUMN)                    
* TABLE IS TO BE OUTPUT (VERSION CONTROL TEST). IF NOT, A FLAG IS SET           
* ("X" IN ROUTFLG) TO BE USED IN LOADBUY AND UPDTBUY PROC'S TO PASS             
* THIS INFO TO PTXROUTS TO TELL IT TO NOT PRODUCE THE BUY CUSTOM                
* COLUMN DATA RECORD                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    PTXPARM(PTXPARMLQ),PTXPARM                                       
         XC    SVAMCLT,SVAMCLT                                                  
         MVI   ROUTFLG,C' '        CLEAR FLAG                                   
         LA    R3,LOADTAB                                                       
CCCHK    DS    0H                                                               
         CLI   0(R3),0                                                          
         JNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         CLC   =C'CCC',0(R3)       CUSTOM COLUMN TABLE ENTRY ?                  
         JNE   CCCHKUP             TEST NEXT                                    
         CLC   VERSION,3(R3)       OUTPUT CUSTOM COLUMN TABLE ?                 
         BNL   CCCHKX              YES                                          
         MVI   ROUTFLG,C'X'        NO - SAVE FOR LOADBUY & UPDTBUY              
         J     CCCHKX                                                           
CCCHKUP  DS    0H                                                               
         LA    R3,L'LOADTAB(R3)                                                 
         J     CCCHK               NEXT                                         
*                                                                               
CCCHKX   DS    0H                                                               
*                                  MAKE SURE DXFLAGS NOT SET FOR                
*                                  "ALL AGENCIES" IF NOT IDESK REQUEST          
         CLC   SXDTTYP,=C'IDK'     IDESK REQUEST ?                              
         JE    *+8                 YES - LEAVE FLAG ALONE                       
         NI    DXFLAGS,X'FF'-DXFAAGY      TURN OFF "ALL AGENCIES" FLAG          
*                                                                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVI   4(RE),X'0A'         CONTROL                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,CNTROL,CFLIST,IO                            
*                                                                               
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,PRINT,FILELIST,IO                           
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,PRINT,0,IO                                  
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
PRINT    DC    CL8'PRINT'                                                       
CNTROL   DC    CL8'CONTROL'                                                     
*                                                                               
FILELIST DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    CL8'NPUBDIR'                                                     
         DC    CL8'NPUBFILE'                                                    
         DC    CL10'X'                                                          
*                                                                               
CFLIST   DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL10'X       '                                                   
*                                                                               
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN LOAD MODE                              *         
***********************************************************************         
PROCLOAD NTR1  ,                                                                
         MVC   PRIALPHA,SXDTAGY    SET PRIALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
*                                                                               
         CLC   TYPECODE,=C'SUP'    TEST FOR SUPPORT MACRO TYPE                  
         BNE   PROCL20             NO                                           
*                                                                               
         L     R2,ATYPTAB          R2=A(TYPE TABLE)                             
         USING TYPTABD,R2                                                       
         BRAS  RE,PRIINIT                                                       
*                                                                               
* LOOP THROUGH TYPE TABLE FINDING TYPES THAT COMPRISE THE SUPPORT               
* MACRO TYPE AND CALL EACH ONE'S LOAD ROUTINE                                   
*                                                                               
PROCL5   CLI   TYPNAME,X'FF'       TEST FOR E-O-T                               
         BE    YES                 YES-EXIT                                     
*                                                                               
         TM    TYPSTAT,TYPSSUP     TEST FOR SUPPORT EXTRACT TYPE                
         BZ    PROCL10                                                          
*                                                                               
         MVC   TYPECODE,TYPNAME    SET TYPE CODE                                
         GOTO1 AGETTYP                                                          
*                                                                               
         L     RF,TYPEALOD         CALL LOAD ROUTINE FOR TYPE                   
         BASR  RE,RF                                                            
         BNE   NO                                                               
*                                                                               
PROCL10  AHI   R2,TYPTABLQ         POINT TO NEXT TABLE ENTRY                    
         B     PROCL5                                                           
*                                                                               
* SINGLE TYPE VALUE - VALIDATE IT AND INVOKE LOAD ROUTINE                       
*                                                                               
PROCL20  GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
*                                                                               
         BRAS  RE,PRIINIT                                                       
*                                                                               
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN UPDATE MODE READ RECOVERY FILES        *         
***********************************************************************         
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
*                                                                               
         MVC   PRIALPHA,SXDTAGY    SET PRIALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
*                                                                               
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
*                                                                               
         CLI   TYPEFLAG,X'02'                                                   
         JE    PUBCASE                                                          
*                                                                               
         CLI   TYPEFLAG,X'01'                                                   
         JE    UPDT1                                                            
*                                                                               
         CLI   RFILTY,PRIFILQ      TEST PRTFIL RECORD TYPE                      
         JE    UPDTA                                                            
         CLI   RFILTY,PRIDIRQ      TEST PRTDIR RECORD TYPE                      
         JNE   YES                 NEITHER - IGNORE RECORD                      
*                                                                               
UPDTA    DS    0H                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    UPDT2               YES                                          
         CLC   PRIALPHA,RECVHDR+L'RECVHDR                                       
         JNE   YES                 IGNORE RECORD                                
         J     UPDT2                                                            
*                                                                               
UPDT1    DS    0H                                                               
         CLI   RFILTY,PUBFILQ      PUB PRTPUB FILE RECORD TYPE                  
         JNE   YES                 ELSE IGNORE RECORD                           
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    UPDT2               YES                                          
         CLC   PRIALPHA,RECVHDR+L'RECVHDR+7                                     
         JNE   YES                                                              
*                                                                               
UPDT2    DS    0H                                                               
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                  IGNORE RECORD                               
*                                                                               
         BRAS  RE,PRIINIT                                                       
*                                                                               
         L     RF,TYPEAUPD          CALL UPDATE PROCESS ROUTINE                 
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   NO                  EXIT ERROR                                   
         J     YES                 EXIT OK                                      
*                                                                               
PUBCASE  DS    0H                                                               
         CLI   RFILTY,PRIFILQ                                                   
         JE    UPDT2                                                            
         CLI   RFILTY,PUBFILQ                                                   
         JE    UPDT2                                                            
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    UPDT2               YES                                          
         CLC   PRIALPHA,RECVHDR+L'RECVHDR+7                                     
         JE    UPDT2                                                            
         CLC   PRIALPHA,RECVHDR+L'RECVHDR                                       
         JE    UPDT2                                                            
         J     YES                                                              
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         USING RECDS,R5                                                         
RX       USING PAGYREC,RECVHDR+L'RECVHDR                                        
PROCKEY  NTR1  ,                                                                
         CLI   RFILTY,PRIDIRQ      TEST PRTDIR RECORD TYPE                      
         JNE   PKEYC                                                            
*                                  MUST BE PRTDIR RECORD TYPE                   
         TM    RX.PAGYLEN,X'80'  PAGYLEN=KEY+25 (DIRECTORY CNTL BYTES)          
         JZ    PKEY02              NO                                           
         J     PKEYF                                                            
*                                                                               
PKEYC    DS    0H                                                               
         TM    RX.PAGYCNTL,X'80'      IS THIS RECORD DELETED?                   
         JZ    PKEY02              NO                                           
*                                                                               
PKEYF    DS    0H                                                               
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         CLI   RFILTY,PRIDIRQ      TEST PRTDIR RECORD TYPE                      
         JNE   PKEYJ                                                            
*                                 PAGYLEN=KEY+25 (DIRECTORY CNTL BYTES)         
         TM    L'RECVHDR+PAGYLEN-PAGYREC+4(R4),X'80'                            
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         J     PKEYN                                                            
*                                                                               
PKEYJ    DS    0H                                                               
*                                                                               
         TM    L'RECVHDR+PAGYCNTL-PAGYREC+4(R4),X'80'                           
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
*                                                                               
PKEYN    DS    0H                                                               
*                                                                               
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
PKEY02   DS    0H                                                               
         CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
*                                                                               
         L     R4,DXACPYB                                                       
         CLI   RFILTY,PRIDIRQ      TEST PRTDIR RECORD TYPE                      
         JNE   PKEY02D             MUST BE PRTFIL RECORD TYPE                   
*                                                                               
         TM    L'RECVHDR+PAGYLEN-PAGYREC+4(R4),X'80'                            
         JZ    YES                                                              
         J     PKEY02H                                                          
*                                                                               
PKEY02D  DS    0H                                                               
         TM    L'RECVHDR+PAGYCNTL-PAGYREC+4(R4),X'80'                           
         JZ    YES                                                              
*                                                                               
PKEY02H  DS    0H                                                               
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  RX,R5                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
****  NOTE - ADDRESS MUST BE SYNCHRONOUS WITH ADDRESSD DSECT  *********         
***********************************************************************         
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(PTXBCNVX)                                                      
         DC    V(HEXOUT)                                                        
*                                                                               
         DC    V(PRIAGY)          MEDIA/AGENCY                                  
         DC    V(PRICNT)          CLIENT                                        
         DC    V(PRIDIV)          DIVISION                                      
         DC    V(PRIPRD)          PRODUCT                                       
         DC    V(PRIEST)          ESTIMATE                                      
         DC    V(PRIPUB)          PUB                                           
         DC    V(PRICON)          CONTRACT                                      
         DC    V(PRIREP)          REP                                           
         DC    V(PRIJOB)          JOB                                           
         DC    V(PRIBUY)          BUY                                           
         DC    V(PRIPGD)          PUB GROUP DEFINITION                          
         DC    V(PRIPBG)          PUB GROUP                                     
         DC    V(PRIPGP)          PUB GROUP PUB                                 
         DC    V(PRICCC)          CUSTOM COLUMN                                 
         DC    V(PRICGD)          CLIENT GROUP DEFINITION                       
         DC    V(PRICLG)          CLIENT GROUP                                  
         DC    V(PRICLC)          CLIENT GROUP CLIENT                           
         DC    V(PRIGDP)          PRODUCT GROUP DEFINITION                      
         DC    V(PRIGGP)          PRODUCT GROUP                                 
         DC    V(PRIGPP)          PRODUCT GROUP PRODUCT                         
         DC    V(PRIREG)          REGION                                        
         DC    V(PRIDST)          DISTRICT                                      
         DC    V(PRIPDA)          PUB DISTRICT ASSIGNMENT                       
         DC    V(PRIACG)          ADDITIONAL CHARGE                             
         DC    V(PRIUCM)          UCOMM (USER COMMENT)                          
         DC    V(PRIBUD)          BUDGET                                        
*                                                                               
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)                                                       
         DC    A(ACCUPDT)                                                       
         DC    A(DECIOC)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(RECCMP)                                                        
*                                                                               
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADAGY)          MEDIA/AGENCY                                 
         DC    A(LOADCNT)          CLIENT                                       
         DC    A(LOADDIV)          DIVISION                                     
         DC    A(LOADPRD)          PRODUCT                                      
         DC    A(LOADEST)          ESTIMATE                                     
         DC    A(LOADPUB)          PUB                                          
         DC    A(LOADCON)          CONTRACT                                     
         DC    A(LOADREP)          REP                                          
         DC    A(LOADJOB)          JOB                                          
         DC    A(LOADBUY)          BUY                                          
         DC    A(LOADPGD)          PUB GROUP DEFINITION                         
         DC    A(LOADPBG)          PUB GROUP                                    
         DC    A(LOADPGP)          PUB GROUP PUB                                
         DC    A(LOADCCC)          CUSTOM COLUMN                                
         DC    A(LOADCGD)          CLIENT GROUP DEFINITION                      
         DC    A(LOADCLG)          CLIENT GROUP                                 
         DC    A(LOADCLC)          CLIENT GROUP CLIENT                          
         DC    A(LOADGDP)          PRODUCT GROUP DEFINITION                     
         DC    A(LOADGGP)          PRODUCT GROUP                                
         DC    A(LOADGPP)          PRODUCT GROUP PRODUCT                        
         DC    A(LOADREG)          REGION                                       
         DC    A(LOADDST)          DISTRICT                                     
         DC    A(LOADPDA)          PUB DISTRICT ASSIGNMENT                      
         DC    A(LOADACG)          ADDITIONAL CHARGE                            
         DC    A(LOADUCM)          UCOMM (USER COMMENT)                         
         DC    A(LOADBUD)          BUDGET                                       
         DC    A(LOADBLK)          BLOCKCHAIN BUY RECORD                        
*                                                                               
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTAGY)          MEDIA/AGENCY                                 
         DC    A(UPDTCNT)          CLIENT                                       
         DC    A(UPDTDIV)          DIVISION                                     
         DC    A(UPDTPRD)          PRODUCT                                      
         DC    A(UPDTEST)          ESTIMATE                                     
         DC    A(UPDTPUB)          PUB                                          
         DC    A(UPDTCON)          CONTRACT                                     
         DC    A(UPDTREP)          REP                                          
         DC    A(UPDTJOB)          JOB                                          
         DC    A(UPDTBUY)          BUY                                          
         DC    A(UPDTPGD)          PUB GROUP DEFINITION                         
         DC    A(UPDTPBG)          PUB GROUP                                    
         DC    A(UPDTPGP)          PUB GROUP PUB                                
         DC    A(UPDTCCC)          CUSTOM COLUMN                                
         DC    A(UPDTCGD)          CLIENT GROUP DEFINITION                      
         DC    A(UPDTCLG)          CLIENT GROUP                                 
         DC    A(UPDTCLC)          CLIENT GROUP CLIENT                          
         DC    A(UPDTGDP)          PRODUCT GROUP DEFINITION                     
         DC    A(UPDTGGP)          PRODUCT GROUP                                
         DC    A(UPDTGPP)          PRODUCT GROUP PRODUCT                        
         DC    A(UPDTREG)          REGION                                       
         DC    A(UPDTDST)          DISTRICT                                     
         DC    A(UPDTPDA)          PUB DISTRICT ASSIGNMENT                      
         DC    A(UPDTACG)          ADDITIONAL CHARGE                            
         DC    A(UPDTUCM)          UCOMM (USER COMMENT)                         
         DC    A(UPDTBUD)          BUDGET                                       
         DC    A(UPDTBLK)          BLOCKCHAIN BUY RECORD                        
*                                                                               
         DC    CL8'FILTERS'                                                     
         DC    A(FILTAGY)          MEDIA/AGENCY                                 
         DC    A(FILTCNT)          CLIENT                                       
         DC    A(FILTDIV)          DIVISION                                     
         DC    A(FILTPRD)          PRODUCT                                      
         DC    A(FILTEST)          ESTIMATE                                     
         DC    A(FILTPUB)          PUB                                          
         DC    A(FILTCON)          CONTRACT                                     
         DC    A(FILTREP)          REP                                          
         DC    A(FILTJOB)          JOB                                          
         DC    A(FILTBUY)          BUY                                          
         DC    A(FILTPGD)          PUB GROUP DEFINITION                         
         DC    A(FILTPBG)          PUB GROUP                                    
         DC    A(FILTPGP)          PUB GROUP PUB                                
         DC    A(FILTCCC)          CUSTOM COLUMN                                
         DC    A(FILTCGD)          CLIENT GROUP DEFINITION                      
         DC    A(FILTCLG)          CLIENT GROUP                                 
         DC    A(FILTCLC)          CLIENT GROUP CLIENT                          
         DC    A(FILTGDP)          PRODUCT GROUP DEFINITION                     
         DC    A(FILTGGP)          PRODUCT GROUP                                
         DC    A(FILTGPP)          PRODUCT GROUP PRODUCT                        
         DC    A(FILTREG)          REGION                                       
         DC    A(FILTDST)          DISTRICT                                     
         DC    A(FILTPDA)          PUB DISTRICT ASSIGNMENT                      
         DC    A(FILTACG)          ADDITIONAL CHARGE                            
         DC    A(FILTUCM)          UCOMM (USER COMMENT)                         
         DC    A(FILTBUD)          BUDGET                                       
         DC    A(FILTBLK)          BLOCKCHAIN BUY RECORD                        
*                                                                               
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          GENERAL INITIALISATION                       
         DC    A(INITAGY)          MEDIA/AGENCY                                 
         DC    A(INITCNT)          CLIENT                                       
         DC    A(INITDIV)          DIVISION                                     
         DC    A(INITPRD)          PRODUCT                                      
         DC    A(INITEST)          ESTIMATE                                     
         DC    A(INITPUB)          PUB                                          
         DC    A(INITCON)          CONTRACT                                     
         DC    A(INITREP)          REP                                          
         DC    A(INITJOB)          JOB                                          
         DC    A(INITBUY)          BUY                                          
         DC    A(INITPGD)          PUB GROUP DEFINITION                         
         DC    A(INITPBG)          PUB GROUP                                    
         DC    A(INITPGP)          PUB GROUP PUB                                
         DC    A(INITCCC)          CUSTOM COLUMN                                
         DC    A(INITCGD)          CLIENT GROUP DEFINITION                      
         DC    A(INITCLG)          CLIENT GROUP                                 
         DC    A(INITCLC)          CLIENT GROUP CLIENT                          
         DC    A(INITGDP)          PRODUCT GROUP DEFINITION                     
         DC    A(INITGGP)          PRODUCT GROUP                                
         DC    A(INITGPP)          PRODUCT GROUP PRODUCT                        
         DC    A(INITREG)          REGION                                       
         DC    A(INITDST)          DISTRICT                                     
         DC    A(INITPDA)          PUB DISTRICT ASSIGNMENT                      
         DC    A(INITACG)          ADDITIONAL CHARGE                            
         DC    A(INITUCM)          UCOMM (USER COMMENT)                         
         DC    A(INITBUD)          BUDGET                                       
         DC    A(INITBLK)          BLOCKCHAIN BUY RECORD                        
*                                                                               
         DC    CL7'OPEN'                                                        
         DC    CL7'DMREAD'                                                      
         DC    CL7'DMRSEQ'                                                      
         DC    CL7'DMRDHI'                                                      
         DC    CL7'DMCLSE'                                                      
         DC    CL7'DMFAST'                                                      
         DC    CL7'GETREC'                                                      
         DC    CL7'RECOVER'                                                     
         DC    CL7'CONTROL'                                                     
         DC    CL7'CTFILE'                                                      
         DC    CL7'PRTDIR'                                                      
         DC    CL7'PRTFILE'                                                     
         DC    CL7'PUBDIR'                                                      
         DC    CL7'PUBFILE'                                                     
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    CL1'Y'                                                           
         DC    80C' '                                                           
         DC    PL6'0'                                                           
         DC    CL3' '                                                           
*                                                                               
         LTORG                                                                  
PRIFILQ  EQU   X'42'                                                            
PUBFILQ  EQU   X'43'                                                            
PRIDIRQ  EQU   X'40'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPARATOR CHR                          
FF       EQU   X'FF'                                                            
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    N/D                                                                    
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,02,00,00,00),AL4(LOADALL,UPDTALL)                
*                                                                               
         DC    CL3'OPT',AL1(00,02,00,00,00),AL4(LOADOPT,UPDTOPT)                
*                                                                               
         DC    CL3'MDM',AL1(00,02,00,00,00),AL4(LOADMDM,UPDTMDM)                
*                                                                               
         DC    CL3'IDK',AL1(00,02,00,00,00),AL4(0,UPDTIDK)                      
*                                                                               
         DC    CL3'AGY',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADAGY,UPDTAGY)                                             
         DC    CL3'CNT',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADCNT,UPDTCNT)                                             
         DC    CL3'DIV',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADDIV,UPDTDIV)                                             
         DC    CL3'PRD',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADPRD,UPDTPRD)                                             
         DC    CL3'EST',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADEST,UPDTEST)                                             
         DC    CL3'PUB',AL1(00,01,TYPSSUP,00,00)                                
         DC    AL4(LOADPUB,UPDTPUB)                                             
         DC    CL3'CON',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADCON,UPDTCON)                                             
         DC    CL3'REP',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADREP,UPDTREP)                                             
         DC    CL3'JOB',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADJOB,UPDTJOB)                                             
         DC    CL3'PGD',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADPGD,UPDTPGD)                                             
         DC    CL3'PBG',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADPBG,UPDTPBG)                                             
         DC    CL3'PGP',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADPGP,UPDTPGP)                                             
         DC    CL3'CCC',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADCCC,UPDTCCC)                                             
         DC    CL3'CGD',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADCGD,UPDTCGD)                                             
         DC    CL3'CLG',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADCLG,UPDTCLG)                                             
         DC    CL3'CLC',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADCLC,UPDTCLC)                                             
         DC    CL3'GDP',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADGDP,UPDTGDP)                                             
         DC    CL3'GGP',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADGGP,UPDTGGP)                                             
         DC    CL3'GPP',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADGPP,UPDTGPP)                                             
         DC    CL3'REG',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADREG,UPDTREG)                                             
         DC    CL3'DST',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADDST,UPDTDST)                                             
         DC    CL3'PDA',AL1(00,01,TYPSSUP,00,00)                                
         DC    AL4(LOADPDA,UPDTPDA)                                             
         DC    CL3'ACG',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADACG,UPDTACG)                                             
         DC    CL3'UCM',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADUCM,UPDTUCM)                                             
         DC    CL3'BUY',AL1(00,00,00,00,00)                                     
         DC    AL4(LOADBUY,UPDTBUY)                                             
         DC    CL3'BUD',AL1(00,00,TYPSSUP,00,00)                                
         DC    AL4(LOADBUD,UPDTBUD)                                             
         DC    CL3'BLK',AL1(00,00,00,00,00)                                     
         DC    AL4(LOADBLK,UPDTBLK)                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
GETTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP02   CLI   0(RF),FF            END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP04                                                           
         LA    RF,TYPTABLQ(RF)     GET NEXT ENTRY                               
         B     GTYP02                                                           
*                                                                               
GTYP04   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         JNZ   YES                                                              
*                                                                               
         LA    R3,DECMSG           OUTPUT IO COUNT EXCEEDED MESSAGE             
         MVC   DECTYPE,TYPENAME                                                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
DECMSGL  DC    AL2(50)                                                          
DECMSG   DC    CL50' '                                                          
         ORG   DECMSG                                                           
         DC    C'IO COUNT EXCEEDED - TYPECODE = '                               
DECTYPE  DC    CL3' '                                                           
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       PRIADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
GETIT    NTR1  BASE=*,LABEL=*                                                   
         CLI   PRIPFLG,PUBKIDQ     X'81'                                        
         JE    GETIT05                                                          
         CLI   PRIPFLG,LTLKIDQ     X'85'                                        
         JE    GETIT05                                                          
         J     GETIT10                                                          
GETIT05  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),PUBFIL,PRIADDR,(R2),DMWORK          
         J     GETIT20                                                          
GETIT10  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(ONLYB,GETREC),PRTFIL,PRIADDR,(R2),DMWORK          
GETIT20  DS    0H                                                               
         CLI   8(R1),0                                                          
         JE    YES                                                              
         CLI   8(R1),X'02'      FOR BUY ONLY                                    
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,PRIADDR,GETDA,L'PRIADDR,0                        
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSGL          OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     HIGH                                                             
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         CLI   PRIPFLG,PUBKIDQ     X'81'                                        
         JE    READH05                                                          
         CLI   PRIPFLG,LTLKIDQ     X'85'                                        
         JE    READH05                                                          
         J     READH10                                                          
READH05  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),PUBDIR,IOKEY,(R2),DMWORK            
         CLI   8(R1),0                                                          
         JE    YES                                                              
         J     READH20                                                          
READH10  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(ONLYB,DMRDHI),PRTDIR,IOKEY,(R2),DMWORK            
*                                                                               
         CLI   8(R1),0                                                          
         JE    YES                                                              
         CLI   8(R1),X'02'      FOR BUY ONLY                                    
         JE    YES                                                              
READH20  DS    0H                                                               
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV,0                     
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI ERROR KEY HEXOUT FOLLOWS'                       
*                                                                               
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'KEY='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
         CLI   RFILTY,PRIFILQ      PRINT FILE?                                  
         JNE   NO                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PBUYREC,R2                                                       
         MVC   CHGBKEY,0(R2)       SAVE KEY TO CHECK AGAINST COPY REC           
*                                                                               
         CLI   PBUYKRCD,X'20'                                                   
         JNE   NO                                                               
*                                                                               
         ZAP   CHGBDCOS,=P'0'      INIT SIGNED COST FROM CHANGE RECORD          
         ZAP   CPYBDCOS,=P'0'      INIT SIGNED COST FROM COPY RECORD            
*                                                                               
         MVC   CHGMAUTH,SPACES                                                  
         MVC   CPYMAUTH,SPACES                                                  
*                                                                               
         LA    R0,INVNOTBL         CLEAR INVOICE BUFFER                         
         LHI   R1,INVNOBLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   INVNOTBX,X'FF'      END OF INVOICE BUFFER                        
*                                                                               
         XR    R3,R3               INITIALIZE INVOICE COUNT                     
         LA    R4,INVNOTBL         POINT TO INVOICE NO TABLE                    
         USING INVND,R4                                                         
*                                                                               
         LA    R2,33(R2)           POINT TO FIRST ELEMENT OF RECORD             
RECCMP02 CLI   0(R2),X'00'         END OF THE RECORD?                           
         JE    RECCMP04            YES, READ COPY RECORD                        
         CLI   0(R2),X'CC'         SEARCH FOR CUSTOM COLUMN                     
         JE    RECCMP2B                                                         
         CLI   0(R2),X'20'         BUY DESCRIPTION ELEMENT ID                   
         JE    RECCMP2C                                                         
         CLI   0(R2),X'51'         NO SEARCH FOR ELEMENT PBNVELQ                
         JNE   RECCMP2X            FOUND, CONTINUE                              
*                                                                               
         USING PBNVELMD,R2                                                      
RECCMP2A DS    0H                                                               
         MVC   INVNINV,PBNVINV#    STORE IT IN INV NO TABLE                     
         MVC   INVNSERL,PBNVSER#   SERIAL NUMBER                                
         MVC   INVNSQNO,PBNVDSQN   SEQUENCE NUMBER                              
         J     RECCMP2T                                                         
*                                                                               
         USING BYCCELD,R2                                                       
RECCMP2B CLC   =H'8213',BYCCSQN    CC INV#                                      
         JNE   RECCMP2D            NO, NEXT ELEMENT                             
         MVC   INVNINV,SPACES                                                   
         XR    RF,RF               SAVE CC INVOICE IN INV NO TABLE              
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL+1                                                    
         EX    RF,*+8                                                           
         B     RECCMP2T                                                         
         MVC   INVNINV(0),BYCCDATA                                              
*                                                                               
RECCMP2D CLC   =H'8249',BYCCSQN                                                 
         JNE   RECCMP2X                                                         
         XR    RF,RF               SAVE CC INVOICE IN INV NO TABLE              
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL+1                                                    
         EX    RF,*+8                                                           
         B     RECCMP2X                                                         
         MVC   CHGMAUTH(0),BYCCDATA                                             
*                                                                               
RECCMP2T AHI   R4,INVNLNQ          BUMP TO NEXT ENTRY IN BUFFER                 
         AHI   R3,1                BUMP COUNTER                                 
         CHI   R3,MAXINVNO         IF MORE THAN ALLOWED INVOICES                
         JH    *+2                 OVERFLOW, INCREASE INVBUFF                   
*                                                                               
RECCMP2X LLC   R0,1(R2)            NOT FOUND, BUMP TO NEXT ELEMENT              
         AR    R2,R0                                                            
         J     RECCMP02                                                         
*                                                                               
         USING PBDELEM,R2          BUY DESCRIPTION ELEMENT ID                   
RECCMP2C OC    PBDCOS,PBDCOS       GET SIGNED COST                              
         JZ    *+10                                                             
         ZAP   CHGBDCOS,PBDCOS     GET SIGNED COST                              
         J     RECCMP2X            CHECK FOR NEXT ELEMENT                       
*                                                                               
RECCMP04 CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JNE   RECCMP05                                                         
         LTR   R3,R3               NO INVOICES FOR ADD?                         
         JE    NO                                                               
*                                                                               
RECCMP05 L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         CLI   RFILTY,PRIFILQ      PRINT FILE?                                  
         JNE   NO                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         MVC   CPYBKEY,0(R2)       COMPARE COPY AGAINST CHANGE                  
*                                                                               
         CLI   3(R2),X'20'                                                      
         JNE   NO                                                               
*                                                                               
         XR    R1,R1               MATCHED INVOICES COUNT                       
         LA    R2,33(R2)           POINT TO FIRST ELEMENT OF RECORD             
RECCMP06 CLI   0(R2),X'00'         END OF THE RECORD?                           
         JE    RECCMP08            YES, DONE WITH COPY RECORD                   
         CLI   0(R2),X'CC'         SEARCH FOR CUSTOM COLUMN                     
         JE    RECCMP6D                                                         
         CLI   0(R2),X'20'         BUY DESCRIPTION ELEMENT ID                   
         JE    RECCMP6T                                                         
         CLI   0(R2),X'51'         NO SEARCH FOR ELEMENT PBNVELQ                
         JNE   RECCMP6X            FOUND, CONTINUE                              
*                                                                               
         USING PBNVELMD,R2                                                      
         LA    R4,INVNOTBL         POINT TO INVOICE NO TABLE                    
         J     *+8                                                              
RECCMP6A AHI   R4,INVNLNQ                                                       
         CLI   0(R4),X'FF'         END OF INV NO TABLE                          
         JE    RECCMP6S            YES,CHECK FOR NEXT ELEMENT                   
         MVC   WORK(L'PBNVINV#),PBNVINV#                                        
         CLC   INVNINV,PBNVINV#    NO,MACTHED INV NO?                           
RECCMP6B JNE   RECCMP6A            NO, CHECK NEXT INV NO FROM TBL               
         AHI   R1,1                YES, BUMP MATCHED INVOICE COUNT              
         MVI   INVNSTAT,C'M'       MARK MATCHED                                 
         J     RECCMP6X                                                         
*                                                                               
         USING BYCCELD,R2                                                       
RECCMP6D CLC   =H'8213',BYCCSQN    CC INV#                                      
         JNE   RECCMP6U            NO, NEXT ELEMENT                             
         XR    RF,RF               GET LENGTH OF DATA ONLY FOR CC ELEM          
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL+1                                                    
         LA    R4,INVNOTBL         POINT TO INVOICE NO TABLE                    
         J     *+8                                                              
RECCMP6F AHI   R4,INVNLNQ                                                       
         CLI   0(R4),X'FF'         END OF INV NO TABLE                          
         JE    RECCMP6S            YES,CHECK FOR NEXT ELEMENT                   
         MVC   WORK(L'PBNVINV#),SPACES                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),BYCCDATA    MATCH INV NUMBER?                            
*                                                                               
         CLC   INVNINV,WORK        MATCH INV NUMBER?                            
         JNE   RECCMP6F            NO, CHECK NEXT INV NO FROM TABLE             
         MVI   INVNSTAT,C'M'       MARK MATCHED                                 
         AHI   R1,1                YES, BUMP MATCHED INVOICE COUNT              
         J     RECCMP6X                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         USING PBNVELMD,R2                                                      
RECCMP6S LTR   R0,R3               ADD DELTED ONE TO INVNOTBL                   
         MHI   R0,INVNLNQ                                                       
                                                                                
         USING INVND,RF                                                         
         LA    RF,INVNOTBL                                                      
         AR    RF,R0                                                            
         MVI   INVNSTAT,C'D'                                                    
         MVC   INVNINV,WORK                                                     
         MVC   INVNSERL,PBNVSER#   SERIAL NUMBER                                
         AHI   R3,1                COUNT COPY REC HAS 1 MORE                    
*                                                                               
RECCMP6X LLC   R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         J     RECCMP06                                                         
*                                                                               
         USING PBDELEM,R2          BUY DESCRIPTION ELEMENT ID                   
RECCMP6T OC    PBDCOS,PBDCOS       GET SIGNED COST                              
         JZ    *+10                                                             
         ZAP   CPYBDCOS,PBDCOS     GET SIGNED COST                              
         J     RECCMP6X                                                         
*                                                                               
         USING BYCCELD,R2          MEDIA AUTHORIZATION NUMBER                   
RECCMP6U CLC   =H'8249',BYCCSQN                                                 
         JNE   RECCMP6X                                                         
         XR    RF,RF                                                            
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL+1                                                    
         EX    RF,*+8                                                           
         J     RECCMP6X                                                         
         MVC   CPYMAUTH(0),BYCCDATA                                             
*                                                                               
RECCMP08 LTR   R3,R3               NO INVOICES IN CHANGE RECORD?                
         JNZ   RECCMP8A                                                         
         LTR   R1,R1               NO INOICES IN COPY RECORD?                   
         JZ    NO                                                               
RECCMP8A CR    R3,R1               IF MATCHED INV COUNT IS EQUAL TO             
         JNE   RECCMP8B                                                         
*                                                                               
         CP    CHGBDCOS,CPYBDCOS   SIGNED COST IS DIFFERENT                     
         JNE   RECCMP8C            PROCESS IT                                   
*                                                                               
RECCMP8B CLC   CHGMAUTH,CPYMAUTH   MEDIA AUTH IS DIFFERENT                      
         JNE   RECCMP8C            CHANGED                                      
         CLC   CHGMAUTH,SPACES     PROCESS THE BUY IF MEDIA AUTH IS             
         JH    RECCMP8C            ADDED OR CHANGED                             
         CLC   CPYMAUTH,SPACES                                                  
         JH    NO                                                               
*                                                                               
RECCMP8C LA    RF,INVNOTBL         X'00' STATUS                                 
RECCMP8D CLI   0(RF),X'FF'         EOT                                          
         JE    YES                                                              
         OC    INVNSTAT(INVNLNQ),INVNSTAT     NO MORE ENTRIES                   
         JZ    YES                                                              
         CLI   INVNSTAT,0          ONLY MARK 00 STATUS AS ADD                   
         JNE   *+8                                                              
         MVI   INVNSTAT,C'A'                                                    
         CP    CHGBDCOS,CPYBDCOS   SINGED COST IS DIFFERENT                     
         JNE   RECCMP8E                                                         
         CLC   CHGMAUTH,CPYMAUTH   MEDIA AUTH IS DIFFERENT                      
         JNE   RECCMP8E            CHANGED                                      
         J     RECCMP8F                                                         
*                                                                               
RECCMP8E MVI   INVNSTAT,C'C'                                                    
         CLC   CHGBKEY(PBUYLEN-PBUYRECD),CPYBKEY SKIP PROCESSING OF             
         JE    RECCMP8F                          CHANGE RECORD IF COPY          
         MVI   INVNSTAT,C'S'                     IS UNAVAILABLE                 
                                                                                
RECCMP8F AHI   RF,INVNLNQ                                                       
         J     RECCMP8D                                                         
*                                                                               
         LTORG                                                                  
CHGBKEY  DS    CL33                               CHG RECORD BUY KEY            
CPYBKEY  DS    CL33                               CPY RECORD BUY KEY            
CHGBDCOS DS    PL5                 SIGNED COST FROM CHG REC                     
CPYBDCOS DS    PL5                 SIGNED COST FROM CPY REC                     
CHGMAUTH DS    CL32                MEDIA AUTHORIZATION FROM CHG REC             
CPYMAUTH DS    CL32                MEDIA AUTHORIZATION FROM CPY REC             
*                                                                               
         SPACE                                                                  
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD04   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD02                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
*                                                                               
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'DIV',AL1(0),AL4(LOADDIV) DIVISION                            
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'PUB',AL1(0),AL4(LOADPUB) PUB                                 
         DC    CL3'CON',AL1(0),AL4(LOADCON) CONTRACT                            
         DC    CL3'REP',AL1(0),AL4(LOADREP) REP                                 
         DC    CL3'JOB',AL1(0),AL4(LOADJOB) JOB                                 
         DC    CL3'BUY',AL1(0),AL4(LOADBUY) BUY                                 
         DC    CL3'PGD',AL1(2),AL4(LOADPGD) PUB GROUP DEFINITION                
         DC    CL3'PBG',AL1(2),AL4(LOADPBG) PUB GROUP                           
         DC    CL3'PGP',AL1(2),AL4(LOADPGP) PUB GROUP PUB                       
         DC    CL3'CCC',AL1(2),AL4(LOADCCC) CUSTOM COLUMN                       
         DC    CL3'CGD',AL1(3),AL4(LOADCGD) CLIENT GROUP DEFINITION             
         DC    CL3'CLG',AL1(3),AL4(LOADCLG) CLIENT GROUP                        
         DC    CL3'CLC',AL1(3),AL4(LOADCLC) CLIENT GROUP CLIENT                 
         DC    CL3'GDP',AL1(3),AL4(LOADGDP) PRODUCT GROUP DEFINITION            
         DC    CL3'GGP',AL1(3),AL4(LOADGGP) PRODUCT GROUP                       
         DC    CL3'GPP',AL1(3),AL4(LOADGPP) PRODUCT GROUP PRODUCT               
         DC    CL3'REG',AL1(3),AL4(LOADREG) REGION                              
         DC    CL3'DST',AL1(3),AL4(LOADDST) DISTRICT                            
         DC    CL3'PDA',AL1(3),AL4(LOADPDA) PUB DISTRICT ASSIGNMENT             
         DC    CL3'ACG',AL1(5),AL4(LOADACG) ADDITIONAL CHARGE                   
         DC    CL3'UCM',AL1(6),AL4(LOADUCM) UCOMM (USER COMMENT)                
         DC    CL3'BUD',AL1(0),AL4(LOADBUD) BUDGET                              
         DC    CL3'BLK',AL1(0),AL4(LOADBLK) BLOCKCHAIN                          
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OPT (MASTER DATABASE) DATA                                     *         
***********************************************************************         
LOADOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOPTTAB                                                       
*                                                                               
LOPT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOPT04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOPT04   LA    R3,L'LOPTTAB(R3)                                                 
         J     LOPT02                                                           
*                                                                               
LOPTTAB  DS    0XL8                                                             
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'PUB',AL1(0),AL4(LOADPUB) PUB                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD MDM DATA                                                       *         
***********************************************************************         
LOADMDM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LMDMTAB                                                       
*                                                                               
LMDM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LMDM04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LMDM04   LA    R3,L'LMDMTAB(R3)                                                 
         J     LMDM02                                                           
*                                                                               
LMDMTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'PUB',AL1(0),AL4(LOADPUB) PUB                                 
         DC    CL3'JOB',AL1(0),AL4(LOADJOB) JOB                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT02                                                           
                                                                                
UPDTTAB  DS    0XL8                                                             
         DC    CL3'BUY',AL1(0),AL4(UPDTBUY) BUY                                 
         DC    CL3'PUB',AL1(0),AL4(UPDTPUB) PUB                                 
         DC    CL3'CON',AL1(0),AL4(UPDTCON) CONTRACT                            
         DC    CL3'JOB',AL1(0),AL4(UPDTJOB) JOB                                 
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'DIV',AL1(0),AL4(UPDTDIV) DIVISION                            
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'REP',AL1(0),AL4(UPDTREP) REP                                 
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY                              
         DC    CL3'PGD',AL1(2),AL4(UPDTPGD) PUB GROUP DEFINITION                
         DC    CL3'PBG',AL1(2),AL4(UPDTPBG) PUB GROUP                           
         DC    CL3'PGP',AL1(2),AL4(UPDTPGP) PUB GROUP PUB                       
         DC    CL3'CCC',AL1(2),AL4(UPDTCCC) CUSTOM COLUMN                       
         DC    CL3'CGD',AL1(3),AL4(UPDTCGD) CLIENT GROUP DEFINITION             
         DC    CL3'CLG',AL1(3),AL4(UPDTCLG) CLIENT GROUP                        
         DC    CL3'CLC',AL1(3),AL4(UPDTCLC) CLIENT GROUP CLIENT                 
         DC    CL3'GDP',AL1(3),AL4(UPDTGDP) PRODUCT GROUP DEFINITION            
         DC    CL3'GGP',AL1(3),AL4(UPDTGGP) PRODUCT GROUP                       
         DC    CL3'GPP',AL1(3),AL4(UPDTGPP) PRODUCT GROUP PRODUCT               
         DC    CL3'REG',AL1(3),AL4(UPDTREG) REGION                              
         DC    CL3'DST',AL1(3),AL4(UPDTDST) DISTRICT                            
         DC    CL3'PDA',AL1(3),AL4(UPDTPDA) PUB DISTRICT ASSIGNMENT             
         DC    CL3'ACG',AL1(5),AL4(UPDTACG) ADDITIONAL CHARGE                   
         DC    CL3'UCM',AL1(6),AL4(UPDTUCM) UCOMM (USER COMMENT)                
         DC    CL3'BUD',AL1(0),AL4(UPDTBUD) BUDGET                              
         DC    CL3'BLK',AL1(0),AL4(UPDTBLK) BLOCKCHAIN                          
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
UPDTOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UOPTTAB                                                       
UOPT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UOPT04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UOPT04   LA    R3,L'UOPTTAB(R3)                                                 
         J     UOPT02                                                           
                                                                                
UOPTTAB  DS    0XL8                                                             
         DC    CL3'PUB',AL1(0),AL4(UPDTPUB) PUB                                 
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE MDM DATA                                                     *         
***********************************************************************         
UPDTMDM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UMDMTAB                                                       
UMDM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UMDM04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UMDM04   LA    R3,L'UMDMTAB(R3)                                                 
         J     UMDM02                                                           
                                                                                
UMDMTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'PUB',AL1(0),AL4(UPDTPUB) PUB                                 
         DC    CL3'JOB',AL1(0),AL4(UPDTJOB) JOB                                 
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE IDK RECORD DATA                                              *         
***********************************************************************         
UPDTIDK  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPIDKTAB                                                      
UPDTID2  CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDTID4                                                          
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDTID4  LA    R3,L'UPIDKTAB(R3)                                                
         J     UPDTID2                                                          
                                                                                
UPIDKTAB DS    0XL8                                                             
*                                                                               
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'PUB',AL1(0),AL4(UPDTPUB) PUB                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
*                                                                               
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,DXHDRCDT)                             
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         JNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL02   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         JNE   YES                                                              
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),=C'  '                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LOAD BUY RECORD                                                               
*        ROUTINE WILL BUILD BUY  BUY COMMENT AND BUY SUMMARY                    
*        AND BUY CUSTOM COLUMN                                                  
*---------------------------------------------------------------------*         
LOADBUY  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
         MVI   ONLYB,X'08'         ONLY IN CASE FOR BUY RECORDS                 
LBUY01   CLI   MEDTCODE,C'A'                                                    
         JNE   LBUY010                                                          
         J     YES                                                              
*                                                                               
LBUY010  DS    0H                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CON RECORD             
         USING PBUYREC,R2                                                       
         XC    PBUYKEY,PBUYKEY                                                  
*                                                                               
         MVC   PBUYKAGY,PRIALPHA                                                
         MVC   PBUYKMED,MEDTCODE                                                
         MVI   PBUYKRCD,PBUYKIDQ   X'20'                                        
*                                                                               
         L     R2,DXARECB                                                       
*                                                                               
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
***                                                                             
         CLI   25(R2),X'FF'        READ NEXT RECORD                             
         JE    LBUY08                                                           
*                                                                               
LBUY02   DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         CLC   21(3,R2),=X'000000'                                              
         JNE   LBUY08                                                           
*                                                                               
         GOTO1 AFILTBUY                                                         
         JNE   LBUY08                                                           
*                                                                               
         MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITBUY            INITIALISE EXTRACT BUFFER                    
*                                                                               
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
*  ROUTFLG BELOW IS SET TO "X" IN GENINIT IF THE CUSTOM COLUMN TABLE            
*  IS TO BE EXCLUDED UNDER VERSION CONTROL. IF SO, THE BUY CUSTOM               
*  COLUMN DATA TABLE SHOULD NOT BE PRODUCED IN THE PTXROUTS MODULE              
*  CALLED BY VPRIBUYC BELOW.                                                    
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(1,0),              X        
               (ROUTFLG,(R6)),(VERSION,MEDTABD),ADDRESSD                        
*                                                                               
*                                  GET NEXT UNCOMMITTED RECORD                  
LBUY04   DS    0H                                                               
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(2,0),(R6),MEDTABD, X        
               ADDRESSD                                                         
         CLI   8(R1),X'FF'                                                      
         JE    LBUY08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LBUY08              DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LBUY06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LBUY06   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
LBUY07   DS    0H                                                               
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LBUY04                                                           
*                                                                               
LBUY08   DS    0H                                                               
*                                                                               
         MVC   IOKEY(L'PBUYKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(ONLYB,DMRSEQ),PRTDIR,IOKEY,(R2),DMWORK            
         CLI   25(R2),X'FF'                                                     
         JE    LBUY08                                                           
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   3(R2),PBUYKIDQ                                                   
         JNE   LBUY09                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBUY02                                                           
         J     YES                                                              
*                                                                               
LBUY09   AHI   R8,MEDTLNQ                                                       
         J     LBUY01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***************                                                                 
* UPDTBUY                                                                       
***************                                                                 
UPDTBUY  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'          SPECIAL FLAG TO DIFFER                   
*                                      COPY FROM CHANGE                         
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PBUYREC,R2                                                       
         LA    R1,PBUYKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTBUY                                                         
         JNE   YES                                                              
         GOTO1 AINITBUY                                                         
***********************         *****************                               
* INSERT X'00' AT THE END OF RECORD                                             
***********************         *****************                               
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)                                                      
         AR    R1,R2                                                            
         MVI   0(R1),X'00'                                                      
***********************         *****************                               
         J     UPDTBUY4                                                         
*                                                                               
UPDTBUY4 DS    0H                                                               
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
*  ROUTFLG BELOW IS SET TO "X" IN GENINIT IF THE CUSTOM COLUMN TABLE            
*  IS TO BE EXCLUDED UNDER VERSION CONTROL. IF SO, THE BUY CUSTOM               
*  COLUMN DATA TABLE SHOULD NOT BE PRODUCED IN THE PTXROUTS MODULE              
*  CALLED BY VPRIBUYC BELOW.                                                    
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(1,0),              X        
               (ROUTFLG,(R6)),(R8),ADDRESSD                                     
*                                                                               
UPDTBUY5 DS    0H                                                               
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(2,0),(R6),(R8),    X        
               ADDRESSD                                                         
         CLI   8(R1),X'FF'                                                      
         JE    UPDTBUY9            NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPDTBUY9            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UPDTBUY6            DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UPDTBUY6 DS    0H                                                               
*                                                                               
         CLI   (PXG$ACT-PXG$BUY)(RF),C'A'      IF ADD SKIP TO PUT               
         JE    UPDTBUY8                                                         
         MVI   (PXG$ACT-PXG$BUY)(RF),C'D'                                       
         MVI   COPYFLAG,X'01'                                                   
         J     UPDTBUY8                                                         
*                                                                               
UPDTBUY7 DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   UPDTBUY5                                                         
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   (PXG$ACT-PXG$BUY)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPDTBUY8 DS    0H                                                               
*                                                                               
         CLI   (PXG$ACT-PXG$BUY)(RF),C'D'                                       
         JE    *+22                                                             
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     UPDTBUY7                                                         
*                                                                               
UPDTBUY9 DS    0H                                                               
         J     YES                                                              
*---------------------------------------------------------------------*         
* FILTER BUY REC                                                                
*---------------------------------------------------------------------*         
FILTBUY  NTR1  BASE=*,LABEL=*                                                   
         USING PBUYREC,R2                                                       
         CLC   PBUYKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PBUYKRCD,X'20'                                                   
         JNE   NO                                                               
         CLC   PBUYKDAT,=X'650101'      SKIP IF BEFORE 1/1/2001                 
         JL    NO                                                               
         CLC   PBUYKACT,=X'000000'                                              
         JNE   NO                                                               
************************************* TESTING BELOW                             
*SMY*    CLC   PBUYKDAT,=X'660101'      SKIP IF BEFORE 1/1/2002                 
*SMY*    JL    NO                                                               
*SMY*    CLI   PBUYKMED,C'I'                                                    
*SMY*    JNE   NO                                                               
*SMY*    NOP   *+4                                                              
*SMY*    CLC   PBUYKCLT,=C'AB '                                                 
*SMY*    JNE   NO                                                               
************************************* TESTING ABOVE                             
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE BUY RECORD                                                         
*---------------------------------------------------------------------*         
INITBUY  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PXG$BUYL         R1=L'CON RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LOAD BLK RECORD                                                               
*        ROUTINE WILL BUILD BLK  BLK COMMENT AND BLK SUMMARY                    
*        AND BLK CUSTOM COLUMN                                                  
*---------------------------------------------------------------------*         
LOADBLK  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
         MVI   ONLYB,X'08'         ONLY IN CASE FOR BLK RECORDS                 
LBLK01   CLI   MEDTCODE,C'A'                                                    
         JNE   LBLK010                                                          
         J     YES                                                              
*                                                                               
LBLK010  DS    0H                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CON RECORD             
         USING PBUYREC,R2                                                       
         XC    PBUYKEY,PBUYKEY                                                  
*                                                                               
         MVC   PBUYKAGY,PRIALPHA                                                
         MVC   PBUYKMED,MEDTCODE                                                
         MVI   PBUYKRCD,PBUYKIDQ   X'20'                                        
*                                                                               
         L     R2,DXARECB                                                       
*                                                                               
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
***                                                                             
         CLI   25(R2),X'FF'        READ NEXT RECORD                             
         JE    LBLK08                                                           
*                                                                               
LBLK02   DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         CLC   21(3,R2),=X'000000'                                              
         JNE   LBLK08                                                           
*                                                                               
         GOTO1 AFILTBLK                                                         
         JNE   LBLK08                                                           
*                                                                               
         MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITBLK            INITIALISE EXTRACT BUFFER                    
*                                                                               
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
*  ROUTFLG BELOW IS SET TO "X" IN GENINIT IF THE CUSTOM COLUMN TABLE            
*  IS TO BE EXCLUDED UNDER VERSION CONTROL. IF SO, THE BUY CUSTOM               
*  COLUMN DATA TABLE SHOULD NOT BE PRODUCED IN THE PTXROUTS MODULE              
*  CALLED BY VPRIBUYC BELOW.                                                    
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(1,0),              X        
               (ROUTFLG,(R6)),(VERSION,MEDTABD),ADDRESSD                        
*                                                                               
*                                  GET NEXT UNCOMMITTED RECORD                  
LBLK04   DS    0H                                                               
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(2,0),(R6),MEDTABD, X        
               ADDRESSD                                                         
         CLI   8(R1),X'FF'                                                      
         JE    LBLK08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LBLK08              DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LBLK06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LBLK06   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
LBLK07   DS    0H                                                               
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LBLK04                                                           
*                                                                               
LBLK08   DS    0H                                                               
*                                                                               
         MVC   IOKEY(L'PBUYKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(ONLYB,DMRSEQ),PRTDIR,IOKEY,(R2),DMWORK            
         CLI   25(R2),X'FF'                                                     
         JE    LBLK08                                                           
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   3(R2),PBUYKIDQ                                                   
         JNE   LBLK09                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBLK02                                                           
         J     YES                                                              
*                                                                               
LBLK09   AHI   R8,MEDTLNQ                                                       
         J     LBLK01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***************                                                                 
* UPDTBLK                                                                       
***************                                                                 
UPDTBLK  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'          SPECIAL FLAG TO DIFFER                   
*                                      COPY FROM CHANGE                         
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PBUYREC,R2                                                       
         LA    R1,PBUYKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTBLK                                                         
         JNE   YES                                                              
         GOTO1 AINITBLK                                                         
***********************         *****************                               
* INSERT X'00' AT THE END OF RECORD                                             
***********************         *****************                               
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)                                                      
         AR    R1,R2                                                            
         MVI   0(R1),X'00'                                                      
***********************         *****************                               
         J     UPDTBLK4                                                         
*                                                                               
UPDTBLK4 DS    0H                                                               
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
*  ROUTFLG BELOW IS SET TO "X" IN GENINIT IF THE CUSTOM COLUMN TABLE            
*  IS TO BE EXCLUDED UNDER VERSION CONTROL. IF SO, THE BUY CUSTOM               
*  COLUMN DATA TABLE SHOULD NOT BE PRODUCED IN THE PTXROUTS MODULE              
*  CALLED BY VPRIBUYC BELOW.                                                    
* * * * * * * * * * * * * * * * ** *** * * * * * * * * * * * * * * * *          
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(1,0),              X        
               (ROUTFLG,(R6)),(R8),ADDRESSD                                     
*                                                                               
UPDTBLK5 DS    0H                                                               
*                                                                               
         GOTO1 VPRIBUYC,DMCB,(DXMODE,DXAXREC),(R2),(2,0),(R6),(R8),    X        
               ADDRESSD                                                         
         CLI   8(R1),X'FF'                                                      
         JE    UPDTBLK9            NO MORE RECORDS LEFT                         
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPDTBLK9            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         USING INVND,R3                                                         
         LA    R3,INVNOTBL                                                      
UPDTB5A  CLI   0(R3),X'FF'         END OF INVOICE BUFFER?                       
         JE    UPDTBLK5                                                         
         OC    INVNSTAT(INVNLNQ),INVNSTAT     ANYTHING TO PROCESS?              
         JZ    UPDTBLK5                                                         
*        CLI   COPYFLAG,X'01'                                                   
*        JE    UPDTBLK5                                                         
         OC    INVNSERL,INVNSERL                                                
         JZ    UPDTB5B                                                          
         BRAS  RE,PROCINVS         PROCESS INVOICE RECORD                       
*                                                                               
UPDTB5B  DS    0H                                                               
         L     RF,DXAXREC                                                       
         CLI   INVNSTAT,C'S'       SKIP INVOICE?                                
         BE    UPDTB5C3                                                         
*                                                                               
         CLI   INVNSTAT,C'A'       ALLOW ADDED INVOICES                         
         JNE   UPDTB5C1            YES,CHECK FOR NEXT ELEMENT                   
         MVC   (PXG$ACN-PXG$BUY)(L'PXG$ACN,RF),=CL8'ADD'                        
         J     UPDTB5C5            YES,CHECK FOR NEXT ELEMENT                   
UPDTB5C1 CLI   INVNSTAT,C'D'       ALLOW ADDED INVOICES                         
         JNE   UPDTB5C2            YES,CHECK FOR NEXT ELEMENT                   
         MVC   (PXG$ACN-PXG$BUY)(L'PXG$ACN,RF),=CL8'DELETE'                     
         J     UPDTB5C5            YES,CHECK FOR NEXT ELEMENT                   
*                                                                               
UPDTB5C2 CLI   INVNSTAT,C'C'       ALLOW ADDED INVOICES                         
         JNE   UPDTB5C3            YES,CHECK FOR NEXT ELEMENT                   
         MVC   (PXG$ACN-PXG$BUY)(L'PXG$ACN,RF),=CL8'CHANGE'                     
         J     UPDTB5C5            YES,CHECK FOR NEXT ELEMENT                   
*                                                                               
UPDTB5C3 AHI   R3,INVNLNQ          BUMP TO NEXT INVOICE IN BUFFER               
         J     UPDTB5A                                                          
*                                                                               
UPDTB5C5 DS    0H                                                               
         MVC   (PXG$INV-PXG$BUY)(L'PBNVINV#,RF),INVNINV                         
*&&DO                                                                           
         CLC   =C'1711',INVNINV                                                 
         JNE   UPDTB5C6                                                         
         J     UPDTB5C6                                                         
*&&                                                                             
UPDTB5C6 CLC   =C'DELETE',(PXG$ACN-PXG$BUY)(RF)          DELETE                 
         JNE   UPDTB5C8                                  AND ZERO               
         OC    (PXG$NET-PXG$BUY)(L'PXG$NET,RF),(PXG$NET-PXG$BUY)(RF)            
         JZ    UPDTB5C3                                  SKIP                   
         CLC   =C'.00',(PXG$NET-PXG$BUY)(RF)     NET=0.00 AND                   
         JE    UPDTB5C3                                  SKIP                   
*                                                                               
*                                                                               
UPDTB5C8 CLC   =C'.00',(PXG$NET-PXG$BUY)(RF)     NET=0.00 AND                   
         JNE   UPDTB5C9                                                         
         BRAS  RE,SKIPBUY        DO WE NEED TO SKIP THIS BUY?                   
         JNE   UPDTB5C9                                                         
         J     UPDTB5C3          SKIP IT                                        
*                                                                               
UPDTB5C9 LA    RE,(PXG$INV-PXG$BUY)(RF)                                         
         MVC   (PXG$VOUC+13-PXG$BUY)(L'PBNVINV#,RF),0(RE)                       
         MVC   (PXG$ACT-PXG$BUY)(1,RF),INVNSTAT                                 
         MVI   PUTRFLAG,X'01'          SPECIAL FLAG TO DIFFER                   
         CLI   (PXG$ACT-PXG$BUY)(RF),C'A'                                       
         JNE   *+8                                                              
         MVI   COPYFLAG,X'01'          SPECIAL FLAG TO DIFFER                   
*                                                                               
UPDTB5D  L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         MVI   PUTCHINV,X'01'          SET FLAG FOR INVOICE FROM CHANGE         
         CLC   (PXG$INV-PXG$BUY)(L'PBNVINV#,RF),SPACES                          
         JH    UPDTB5J                                                          
*                                                                               
         ST    R5,SAVERF                                                        
         L     R5,DXACPYB                                                       
         USING RECDS,R5                                                         
         LA    R5,RECVHDR+L'RECVHDR                                             
*        USING PBUYREC,R5                                                       
*                                                                               
         LA    R5,33(R5)           POINT TO FIRST ELEMENT OF RECORD             
UPDTB5E  CLI   0(R5),X'00'         END OF THE RECORD?                           
         JE    UPDTB5I             YES, READ COPY RECORD                        
         CLI   0(R5),X'CC'         SEARCH FOR CUSTOM COLUMN                     
         JE    UPDTB5G                                                          
         CLI   0(R5),X'51'         NO SEARCH FOR ELEMENT PBNVELQ                
         JNE   UPDTB5H             FOUND, CONTINUE                              
*                                                                               
         USING PBNVELMD,R5                                                      
UPDTB5F  MVC   (PXG$INV-PXG$BUY)(L'PBNVINV#,RF),PBNVINV#                        
         LA    RE,(PXG$INV-PXG$BUY)(RF)                                         
         MVC   (PXG$VOUC+13-PXG$BUY)(L'PBNVINV#,RF),0(RE)                       
         MVI   (PXG$ACT-PXG$BUY)(RF),C'D'                                       
         MVC   (PXG$ACN-PXG$BUY)(L'PXG$ACN,RF),=CL8'DELETE'                     
         MVI   PUTCHINV,X'01'          SPECIAL FLAG TO DIFFER                   
         J     UPDTB5H                                                          
*                                                                               
         USING BYCCELD,R5                                                       
UPDTB5G  CLC   =H'8213',BYCCSQN    CC INV#                                      
         JNE   UPDTB5H             NO, NEXT ELEMENT                             
         MVC   (PXG$INV-PXG$BUY)(L'PBNVINV#,RF),SPACES                          
         XR    RE,RE               SAVE CC INVOICE IN INV NO TABLE              
         IC    RE,BYCCLEN                                                       
         SHI   RE,BYCCHDRL+1                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   (PXG$INV-PXG$BUY)(0,RF),BYCCDATA                                 
         MVC   (PXG$ACN-PXG$BUY)(L'PXG$ACN,RF),=CL8'DELETE'                     
         MVI   PUTCHINV,X'01'          SPECIAL FLAG TO DIFFER                   
         MVI   (PXG$ACT-PXG$BUY)(RF),C'D'                                       
*                                                                               
UPDTB5H  LLC   R0,1(R5)            NOT FOUND, BUMP TO NEXT ELEMENT              
         AR    R5,R0                                                            
         J     UPDTB5E                                                          
*                                                                               
UPDTB5I  L     R5,SAVERF                                                        
         DROP  R5                                                               
                                                                                
UPDTB5J  CLI   SXDTPLFM,0                                                       
         JE    UPDTBLK6            DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UPDTBLK6 DS    0H                                                               
*                                                                               
         MVI   ((PXG$TME-PXG$BUY)-1)(RF),C'|'      IF ADD SKIP TO PUT           
         CLI   (PXG$ACT-PXG$BUY)(RF),C'A'      IF ADD SKIP TO PUT               
         JE    UPDTBLK8                                                         
         MVI   (PXG$ACT-PXG$BUY)(RF),C'D'                                       
         MVI   COPYFLAG,X'01'                                                   
         J     UPDTBLK8                                                         
*                                                                               
UPDTBLK7 DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JE    UPDTB7A                                                          
UPDTB70  AHI   R3,INVNLNQ          BUMP TO NEXT INVOICE IN BUFFER               
         J     UPDTB5A                                                          
UPDTB7A  L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         CLI   PUTCHINV,X'01'          SPECIAL FLAG TO DIFFER                   
         JNE   UPDTB7B                                                          
         MVI   COPYFLAG,X'00'                                                   
         J     UPDTB8A                                                          
UPDTB7B  MVI   (PXG$ACT-PXG$BUY)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPDTBLK8 DS    0H                                                               
*                                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
UPDTB8A  GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         AHI   R3,INVNLNQ          BUMP TO NEXT INVOICE IN BUFFER               
         J     UPDTB5A                                                          
*                                                                               
UPDTBLK9 DS    0H                                                               
         J     YES                                                              
*                                                                               
         DROP  R3                                                               
*---------------------------------------------------------------------*         
* SKIP BUY IF:                                                                  
*   FREE BUY                                                                    
*   COMMISSION FOR PRISMA / RADIA                                               
*---------------------------------------------------------------------*         
SKIPBUY  NTR1                                                                   
         CP    PBDCOS,=P'0'        FREE BUY IS 0.00 OR 0.01                     
         JE    YES                                                              
         CP    PBDCOS,=P'1'                                                     
         JE    YES                                                              
*                                                                               
         CLI   PBDCOSIN,C'C'       COMMISSION                                   
         JNE   NO                                                               
         LA    R4,PBUYRECD+33                                                   
         MVI   ELCODE,PPIDELQ      PID ELEMENT (X'A7')                          
         BRAS  RE,NEXTEL           SAVE SERIAL# AND SEQ# INTO BUFFER            
         JNE   NO                                                               
         USING PPIDELD,R4                                                       
         CLI   PPIDPRG,PPIDPRMQ    PRISMA (X'08')                               
         JE    YES                                                              
         CLI   PPIDPRG,PPIDRADQ    RADIA  (X'09')                               
         JE    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* BUY MULTIPLE INV NUMBERS                                                      
*---------------------------------------------------------------------*         
*&&DO                                                                           
         USING INVND,R6                                                         
BUYINVS  NTR1                                                                   
         XR    R5,R5                                                            
         LA    R6,INVNOTBL                                                      
*                                                                               
         LA    R4,PBUYRECD+33                                                   
         MVI   ELCODE,X'51'        LOOK FOR INVOICE ELEM                        
BUYINVS3 BRAS  RE,NEXTEL           SAVE SERIAL# AND SEQ# INTO BUFFER            
         JNE   BUYINVS5                                                         
         USING PBNVELMD,R4                                                      
         MVC   INVNINV,PBNVINV#                                                 
         MVC   INVNSERL,PBNVSER#                                                
         MVC   INVNSQNO,PBNVDSQN                                                
         AHI   R6,INVNLNQ          BUMP TO NEXT ENTRY IN BUFFER                 
         AHI   R5,1                BUMP COUNTER                                 
         CHI   R5,MAXINVQ                                                       
         JNH   BUYINVS3                                                         
         DC    H'0'                OVERFLOW, INCREASE INVBUFF                   
*                                                                               
BUYINVS5 LA    R4,PBUYRECD+33                                                   
         MVI   ELCODE,X'CC'        LOOK FOR INVOICE ELEM                        
BUYINVS7 BRAS  RE,NEXTEL           SAVE SERIAL# AND SEQ# INTO BUFFER            
         JNE   BUYINVSX                                                         
*                                                                               
         USING BYCCELD,R4                                                       
         CLC   =H'8213',BYCCSQN    CC INV#                                      
         JNE   BUYINVS7            NO, NEXT ELEMENT                             
         MVC   INVNINV,SPACES                                                   
         XR    RF,RF                                                            
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL+1                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INVNINV(0),BYCCDATA                                              
         AHI   R5,1                BUMP COUNTER                                 
         CHI   R5,MAXINVQ                                                       
         JNH   BUYINVS7                                                         
         DC    H'0'                OVERFLOW, INCREASE INVBUFF                   
*                                                                               
BUYINVSX CHI   R5,0                HAVE INVOICES?                               
         JH    YES                 YES                                          
         J     NO                  NO                                           
*                                                                               
         DROP  R6                                                               
*&&                                                                             
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* PROCESS INVOICE REORD                                                         
* R3 POINTS CURRENT ENTRY OF INVOICE SERIAL AND SEQ# TO BE PROCESSED            
*---------------------------------------------------------------------*         
                                                                                
PROCINVS NTR1  BASE=*,LABEL=*,WORK=(R5,PRCIVWLQ)                                
*                                                                               
         USING INVND,R3                                                         
         USING PRCIVWSD,R5         ESTABLISH LOCAL WORKING STORAGE AREA         
         L     R6,DXAXREC          EXTRACTED RECORD WITH BUY FIELDS             
         USING PXG$BUY,R6                                                       
*                                                                               
         BAS   RE,CHKPRSMA         CHECK IF PRISMA INVOICE                      
*                                                                               
         XC    PIVKEY,PIVKEY                                                    
         LA    RE,PIVKEY                                                        
         USING PNVKEY,RE                                                        
         MVC   PNVKAGY,PXG$AGY                                                  
         MVC   PNVKMED,PXG$MED                                                  
         MVI   PNVKRCD,PNVKRCDQ                                                 
         MVC   PNVKSER#,INVNSERL                                                
         MVC   PIVSVKEY,PIVKEY                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),PRTDIR,PIVKEY,PIVKEY                
         CLC   PIVKEY(PNVKELMK-PNVKEY),PIVSVKEY                                 
         JNE   EXIT                EXIT IF KEY NOT ON DIRECTORY                 
*                                                                               
         LA    RF,IO                                                            
         GOTO1 VDATAMGR,DMCB,(X'08',GETREC),PRTFIL,PIVKEY+27,(RF),     +        
               PIVDWORK                                                         
         CLI   8(R1),2             DELETE IS OK                                 
         JE    PRCIV20                                                          
         CLI   8(R1),0                                                          
         JNE   EXIT                EXIT IF RECORD NOT ON FILE                   
*                                                                               
PRCIV20  LA    RF,IO+33                                                         
         CLI   0(RF),X'10'         INVOICE HEADER ELEMENT?                      
         JNE   EXIT                                                             
         USING PNVHDRD,RF                                                       
         MVC   PIV_INV#,PNVHINV#   INVOICE NUMBER                               
         MVC   PIV_IVDT,PNVHDATE   INVOICE DATE                                 
         MVC   PIV_IEDT,PNVHEND    INVOICE END DATE FOR MOS                     
* AS PER KEVIN, COMMENT OUT CHECKING PRISMA INVOICE ENABLED (MAR29/19)          
*        CLI   PRSMINV,C'Y'        PRISMA INVOICE ENABLED?                      
*        BNE   PRCIV30             NO, DON'T BOTHER CHECKING FURTHER            
         CLI   PNVHIVSR,INVADBYQ   ADBUYER INVOICE, X'00' AND C'A'              
         BE    PRCIV30                                                          
         CLI   PNVHIVSR,INVADB_Q                                                
         BE    PRCIV30                                                          
         MVI   INVNSTAT,C'S'       SKIP IF NOT ADBUYER INVOICE                  
         DROP  RF                                                               
*                                                                               
PRCIV30  LLC   RE,1(RF)                                                         
         AR    RF,RE               DUMP TO NEXT ELEMENT                         
*                                                                               
         CLI   0(RF),0             END OF RECORD?                               
         JE    EXIT                                                             
         USING PNVDTLD,RF                                                       
         CLI   PNVDKCDE,PNVDKIDQ   INVOICE ITEM ELEMENT CODE?                   
         JNE   PRCIV30                                                          
         CLI   PNVDKTYP,PNVDKDSQ   INVOICE ITEM DESCRIPTION ELEM?               
         JNE   PRCIV30                                                          
         CLC   PNVDKSQN,INVNSQNO   MATCHING INVOCIE ITEM SEQ#?                  
         JNE   PRCIV30                                                          
         ZAP   PIV_DNET,=P'0'      DEFAULT TO ZERO AMOUNT                       
         OC    PNVDNET,PNVDNET     HAVE INVOICE ITEM NET AMOUNT?                
         JZ    *+10                                                             
         ZAP   PIV_DNET,PNVDNET    INVOICE ITEM NET AMOUNT                      
         DROP  RF                                                               
*                                                                               
         MVC   PXG$INV(L'PIV_INV#),PIV_INV#                                     
         GOTO1 VDATCON,DMCB,(3,PIV_IEDT),(20,PIVDUB1)                           
         MVC   PXG$MOS+0(2),PIVDUB1+2     YY                                    
         MVC   PXG$MOS+2(2),PIVDUB1+4     MM                                    
         EDIT  PIV_DNET,PXG$NET,2,ALIGN=LEFT                                    
         GOTO1 VDATCON,DMCB,(3,PIV_IVDT),(20,PIVDUB1)                           
         MVC   PXG$TRDT+0(2),PIVDUB1+6   DD                                     
         MVC   PXG$TRDT+2(2),PIVDUB1+4   MM                                     
         MVC   PXG$TRDT+4(4),PIVDUB1+0   YYYY                                   
*                                                                               
         J     EXIT                                                             
PRSMINV  DS    C                                                                
*                                                                               
*---------------------------------------------------------------------*         
* CHECK IF PRISMA INVOICE ENABLED FOR BUY                                       
*---------------------------------------------------------------------*         
CHKPRSMA NTR1                                                                   
         MVI   PRSMINV,C'N'        PRISMA INVOICE ENABLED RESET                 
*                                                                               
         LA    R4,PBUYRECD+33                                                   
         MVI   ELCODE,X'B0'        LOOK FOR BUY PRISMA UPLOAD ELEM              
         BRAS  RE,NEXTEL                                                        
         JNE   YES                                                              
         USING PBYDKELD,R4                                                      
         TM    PBYDKST4,BYPRMIVQ   PRISMA INVOICE ENABLED?                      
         JZ    YES                                                              
         MVI   PRSMINV,C'Y'        PRISMA INVOICE ENABLED                       
         J     YES                                                              
         DROP  R3,R4,R5,R6,RB                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER BUY REC                                                                
*---------------------------------------------------------------------*         
         USING SXDTABD,R6                                                       
FILTBLK  NTR1  BASE=*,LABEL=*                                                   
         USING PBUYREC,R2                                                       
         CLC   PBUYKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PBUYKRCD,X'20'                                                   
         JNE   NO                                                               
         CLC   PBUYKDAT,=X'650101'      SKIP IF BEFORE 1/1/2001                 
         JL    NO                                                               
         CLC   PBUYKACT,=X'000000'                                              
         JNE   NO                                                               
*---------------------------------------------------------------------*         
*        CLC   =X'E4C2C920C1F5F0C3D6D5007120070000760C01',PBUYREC               
*        CLC   =X'E4C2C920C1C3C4C1C240007111920000760501',PBUYREC               
*        JNE   *+8                                                              
*        JE    *+4                                                              
*---------------------------------------------------------------------*         
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE BLK RECORD                                                         
*---------------------------------------------------------------------*         
INITBLK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,PXG$BUYL         R1=L'CON RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD AGENCY RECORDS                                                           
*---------------------------------------------------------------------*         
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LAGY01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PAGYREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,PRIALPHA                                                
         MVI   PAGYKRCD,PAGYKIDQ                                                
         MVC   PAGYKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
***********************************************************************         
**       BELOW TEST ADDED TO HANDLE CONTIGUOUS AGENCY RECORDS:                  
**       I.E. A MEDIA O AGENCY RECORD IMMEDIATELY FOLLOWED BY                   
**            A MEDIA S AGENCY RECORD                                           
**            WITH NO OTHER RECORD TYPES BETWEEN THE TWO                        
*                                                                               
         CLC   2(1,R2),MEDTCODE    SAME MEDIA ?                                 
         BNE   LAGY03              NO - BUMP                                    
***********************************************************************         
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIAGYC,AINITAGY,AFILTAGY,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PAGYKIDQ                                                 
         JNE   LAGY03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY02                                                           
         J     YES                                                              
*                                                                               
LAGY03   AHI   R8,MEDTLNQ                                                       
         J     LAGY01                                                           
         EJECT ,                                                                
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE AGENCY RECORD DATA                                                     
*---------------------------------------------------------------------*         
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PAGYREC,R2                                                       
         LA    R1,PAGYKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
         GOTO1 AINITAGY                                                         
         GOTO1 AACCUPDT,DMCB,VPRIAGYC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER AGY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING PAGYREC,R2                                                       
         CLC   PAGYKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PAGYKRCD,X'01'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE AGENCY RECORD                                                      
*---------------------------------------------------------------------*         
INITAGY  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIMDDL          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORDS                                                           
*---------------------------------------------------------------------*         
LOADCNT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LCNT01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PCLTREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PCLTKEY,PCLTKEY                                                  
         MVC   PCLTKAGY,PRIALPHA                                                
*                                                                               
         MVC   PCLTKMED,MEDTCODE                                                
*                                                                               
         MVI   PCLTKRCD,PCLTKIDQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCNT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRICNTC,AINITCNT,AFILTCNT,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PCLTKIDQ                                                 
         JNE   LCNT03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCNT02                                                           
         J     YES                                                              
*                                                                               
LCNT03   AHI   R8,MEDTLNQ                                                       
         J     LCNT01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CNT RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTCNT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PCLTREC,R2                                                       
         LA    R1,PCLTKMED                                                      
         BRAS  RE,GETMED                                                        
         GOTO1 AFILTCNT                                                         
         JNE   YES                                                              
         GOTO1 AINITCNT                                                         
         GOTO1 AACCUPDT,DMCB,VPRICNTC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CNT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTCNT  NTR1  BASE=*,LABEL=*                                                   
         USING PCLTREC,R2                                                       
         CLI   PCLTKRCD,X'02'                                                   
         JNE   NO                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    YES                 YES                                          
         CLC   PCLTKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE CNT RECORD                                                         
*---------------------------------------------------------------------*         
INITCNT  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRICNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD DIVISION RECORDS                                                         
*---------------------------------------------------------------------*         
LOADDIV  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LDIV01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PDIVREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PDIVKEY,PDIVKEY                                                  
         MVC   PDIVKAGY,PRIALPHA                                                
*                                                                               
         MVI   PDIVKRCD,PDIVKIDQ                                                
*                                                                               
         MVC   PDIVKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDIV02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIDIVC,AINITDIV,AFILTDIV,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PDIVKIDQ                                                 
         JNE   LDIV03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDIV02                                                           
         J     YES                                                              
*                                                                               
LDIV03   AHI   R8,MEDTLNQ                                                       
         J     LDIV01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE DIVISION RECORD DATA                                                   
*---------------------------------------------------------------------*         
UPDTDIV  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PDIVREC,R2                                                       
         LA    R1,PDIVKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTDIV                                                         
         JNE   YES                                                              
         GOTO1 AINITDIV                                                         
         GOTO1 AACCUPDT,DMCB,VPRIDIVC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER DIVISION RECORD AT R2                                        *         
*---------------------------------------------------------------------*         
FILTDIV  NTR1  BASE=*,LABEL=*                                                   
         USING PDIVREC,R2                                                       
         CLC   PDIVKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PDIVKRCD,X'03'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE DIVISION RECORD                                                    
*---------------------------------------------------------------------*         
INITDIV  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIDVDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRD RECORDS                                                              
*---------------------------------------------------------------------*         
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LPRD01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PPRDREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PPRDKEY,PPRDKEY                                                  
         MVC   PPRDKAGY,PRIALPHA                                                
         MVI   PPRDKRCD,PPRDKIDQ                                                
         MVC   PPRDKMED,MEDTCODE                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIPRDC,AINITPRD,AFILTPRD,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PPRDKIDQ                                                 
         JNE   LPRD03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD02                                                           
         J     YES                                                              
*                                                                               
LPRD03   AHI   R8,MEDTLNQ                                                       
         J     LPRD01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRD RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PPRDREC,R2                                                       
         LA    R1,PPRDKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
*                                                                               
         CLI   DXMODE,DXUPDTQ                                                   
         BNE   UPDTPRD5                                                         
* IF UPDATING PRODUCT READ AAA RECORD TO GET BILL FORMULA                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PPRDKEY),0(R2)                                             
         MVC   KEY+PPRDKPRD-PPRDKEY(L'PPRDKPRD),=C'AAA'                         
         GOTO1 =A(GETBFM),DMCB,KEY,SVAAABFM                                     
*                                                                               
UPDTPRD5 DS    0H                                                               
         GOTO1 AINITPRD                                                         
         GOTO1 AACCUPDT,DMCB,VPRIPRDC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
*---------------------------------------------------------------------*         
* FILTER PRD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING PPRDREC,R2                                                       
         CLI   PPRDKRCD,X'06'                                                   
         JNE   NO                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    YES                 YES                                          
         CLC   PPRDKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRD RECORD                                                         
*---------------------------------------------------------------------*         
INITPRD  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIPDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
* LOAD JOB RECORDS                                                              
*---------------------------------------------------------------------*         
LOADJOB  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LJOB01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PJOBREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PJOBKEY,PJOBKEY                                                  
         MVC   PJOBKAGY,PRIALPHA                                                
*                                                                               
         MVI   PJOBKRCD,PJOBKIDQ                                                
*                                                                               
         MVC   PJOBKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LJOB02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIJOBC,AINITJOB,AFILTJOB,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PJOBKIDQ                                                 
         JNE   LJOB03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LJOB02                                                           
         J     YES                                                              
*                                                                               
LJOB03   AHI   R8,MEDTLNQ                                                       
         J     LJOB01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE JOB RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTJOB  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PJOBREC,R2                                                       
         LA    R1,PJOBKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTJOB                                                         
         JNE   YES                                                              
         GOTO1 AINITJOB                                                         
         GOTO1 AACCUPDT,DMCB,VPRIJOBC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER JOB RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTJOB  NTR1  BASE=*,LABEL=*                                                   
         USING PJOBREC,R2                                                       
         CLC   PJOBKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PJOBKRCD,X'15'                                                   
         JNE   NO                                                               
         CLC   16(6,R2),=X'000000000000'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE JOB RECORD                                                         
*---------------------------------------------------------------------*         
INITJOB  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIJBDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD BUDGET RECORDS                                                           
*---------------------------------------------------------------------*         
LOADBUD  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LBUD01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PBUDREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   PBUDKAGY,PRIALPHA                                                
         MVC   PBUDKMED,MEDTCODE                                                
         MVI   PBUDKRCD,X'18'                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBUD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIBUDC,AINITBUD,AFILTBUD,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         CLI   IOKEY+3,X'18'       PBUDKRCD                                     
         JNE   LBUD03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBUD02                                                           
         J     YES                                                              
*                                                                               
LBUD03   AHI   R8,MEDTLNQ                                                       
         J     LBUD01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE BUDGET RECORD DATA                                                     
*---------------------------------------------------------------------*         
UPDTBUD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PBUDREC,R2                                                       
         LA    R1,PBUDKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTBUD                                                         
         JNE   YES                                                              
         OI    PTXPFLG1,KILLQ+MULTIQ+BLDMOSTQ                                   
         MVI   ELCOUNT,0                                                        
*                                                                               
UPDTBUD10 DS   0H                                                               
         GOTO1 AINITBUD                                                         
         GOTO1 AACCUPDT,DMCB,VPRIBUDC,0,(R8)                                    
         TM    PTXPFLG1,MULTIQ                                                  
         BO    UPDTBUD10                                                        
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER BUDGET RECORD AT R2                                                    
*---------------------------------------------------------------------*         
FILTBUD  NTR1  BASE=*,LABEL=*                                                   
         USING PBUDREC,R2                                                       
         CLI   PBUDKRCD,X'18'                                                   
         JNE   NO                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    YES                 YES                                          
         CLC   PBUDKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* INITIALISE BUDGET RECORD                                                      
*---------------------------------------------------------------------*         
INITBUD  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIBDDL          R1=L'RECORD                                  
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD EST RECORDS                                                              
*---------------------------------------------------------------------*         
LOADEST  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LEST01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PESTREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PESTKEY,PESTKEY                                                  
         MVC   PESTKAGY,PRIALPHA                                                
         MVI   PESTKRCD,PESTKIDQ                                                
         MVC   PESTKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIESTC,AINITEST,AFILTEST,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PESTKIDQ                                                 
         JNE   LEST03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEST02                                                           
         J     YES                                                              
*                                                                               
LEST03   AHI   R8,MEDTLNQ                                                       
         J     LEST01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE EST RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTEST  NTR1  BASE=*,LABEL=*                                                   
         XC    SVPRDBFM,SVPRDBFM                                                
         XC    SVCLTPRD,SVCLTPRD                                                
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PESTREC,R2                                                       
         LA    R1,PESTKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTEST                                                         
         JNE   YES                                                              
         GOTO1 AINITEST                                                         
         GOTO1 AACCUPDT,DMCB,VPRIESTC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER EST RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTEST  NTR1  BASE=*,LABEL=*                                                   
         USING PESTREC,R2                                                       
         CLI   PESTKRCD,X'07'                                                   
         JNE   NO                                                               
*                                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    FILTEST5            YES                                          
         CLC   PESTKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
*                                                                               
FILTEST5 DS    0H                                                               
         CLC   SVCLTPRD,0(R2)      CLIENT/PRODUCT CHANGED?                      
         JE    YES                                                              
         MVC   SVCLTPRD,0(R2)                                                   
         XC    SVPRDBFM,SVPRDBFM   XC PRODUCT AAA'S BILL FORMULA                
*                                                                               
         BRAS  RE,GETPRBFM         GET PRODUCT'S BILL FORMULA                   
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE EST RECORD                                                         
*---------------------------------------------------------------------*         
INITEST  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIESDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CON RECORDS                                                              
*---------------------------------------------------------------------*         
LOADCON  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LCON01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PCONREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PCONKEY,PCONKEY                                                  
         MVC   PCONKAGY,PRIALPHA                                                
         MVI   PCONKRCD,PCONKIDQ                                                
         MVC   PCONKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCON02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRICONC,AINITCON,AFILTCON,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PCONKIDQ                                                 
         JNE   LCON03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCON02                                                           
         J     YES                                                              
*                                                                               
LCON03   AHI   R8,MEDTLNQ                                                       
         J     LCON01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CON RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTCON  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PCONREC,R2                                                       
         LA    R1,PCONKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTCON                                                         
         JNE   YES                                                              
         GOTO1 AINITCON                                                         
         GOTO1 AACCUPDT,DMCB,VPRICONC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CON RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTCON  NTR1  BASE=*,LABEL=*                                                   
         USING PCONREC,R2                                                       
         CLC   PCONKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PCONKRCD,X'10'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE CON RECORD                                                         
*---------------------------------------------------------------------*         
INITCON  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRICTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD REP RECORDS                                                              
*---------------------------------------------------------------------*         
LOADREP  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
*                                                                               
LREP01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PREPREC,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,PRIALPHA                                                
         MVI   PREPKRCD,PREPKIDQ                                                
         MVC   PREPKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIREPC,AINITREP,AFILTREP,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PREPKIDQ                                                 
         JNE   LREP03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREP02                                                           
         J     YES                                                              
*                                                                               
LREP03   AHI   R8,MEDTLNQ                                                       
         J     LREP01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE REP RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTREP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PREPREC,R2                                                       
         LA    R1,PREPKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTREP                                                         
         JNE   YES                                                              
         GOTO1 AINITREP                                                         
         GOTO1 AACCUPDT,DMCB,VPRIREPC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER REP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTREP  NTR1  BASE=*,LABEL=*                                                   
         USING PREPREC,R2                                                       
         CLC   PREPKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PREPKRCD,X'11'                                                   
         JNE   NO                                                               
*                                                                               
* * * * * * * * * *       A/O FEB/2006        * * * * * * * * * * * * *         
*  BELOW WILL SKIP ANY REP RECORD THAT HAS MORE THAN 4 SIGNICANT      *         
*  CHARACTERS IN THE REP CODE PORTION OF THE REP KEY.                 *         
*  THESE ARE OLD OUTDOOR REP'S THAT WERE USED IN TESTING SOME KIND OF *         
*  REP DIFFERENTIATION (PAY, TRAFFIC, ETC.) USING SAME REP CODE.      *         
*         THESE SEEM TO EXIST ONLY ON ADV1 UNDER SJR                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         CLI   PREPKREP+4,X'00'                                                 
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE REP RECORD                                                         
*---------------------------------------------------------------------*         
INITREP  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIRPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PUB GROUP DEFINITION RECORDS                                             
*---------------------------------------------------------------------*         
LOADPGD  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LPGD01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPKEY,GRPKEY                                                    
         MVC   GRPKAGY,PRIALPHA                                                 
         MVI   GRPKRCOD,GRPKBTYQ                                                
         MVC   GRPKMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPGD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIPGDC,AINITPGD,AFILTPGD,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,GRPKBTYQ                                                 
         JNE   LPGD03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPGD02                                                           
         J     YES                                                              
*                                                                               
LPGD03   AHI   R8,MEDTLNQ                                                       
         J     LPGD01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PUB GROUP DEFINITION RECORD DATA                                       
*---------------------------------------------------------------------*         
UPDTPGD  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPKMED                                                       
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTPGD                                                         
         JNE   YES                                                              
         GOTO1 AINITPGD                                                         
         GOTO1 AACCUPDT,DMCB,VPRIPGDC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PUB GROUP DEFINITION RECORD AT R2                            *         
*---------------------------------------------------------------------*         
FILTPGD  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPKRCOD,GRPKBTYQ   PUB GROUP RECORD CODE ?                      
         JNE   NO                                                               
         OC    GRPKCODE,GRPKCODE   DEFINITION RECORD ?                          
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PUB GROUP DEFINITION RECORD                                        
*---------------------------------------------------------------------*         
INITPGD  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIPFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PUB GROUP RECORDS                                                        
*---------------------------------------------------------------------*         
LOADPBG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LPBG01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPKEY,GRPKEY                                                    
         MVC   GRPKAGY,PRIALPHA                                                 
         MVI   GRPKRCOD,GRPKBTYQ                                                
         MVC   GRPKMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPBG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIPBGC,AINITPBG,AFILTPBG,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,GRPKBTYQ                                                 
         JNE   LPBG03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPBG02                                                           
         J     YES                                                              
*                                                                               
LPBG03   AHI   R8,MEDTLNQ                                                       
         J     LPBG01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PUB GROUP RECORD DATA                                                  
*---------------------------------------------------------------------*         
UPDTPBG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPKMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTPBG                                                         
         JNE   YES                                                              
         GOTO1 AINITPBG                                                         
         GOTO1 AACCUPDT,DMCB,VPRIPBGC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PUB GROUP RECORD AT R2                                       *         
*---------------------------------------------------------------------*         
FILTPBG  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPKRCOD,GRPKBTYQ   PUB GROUP RECORD CODE ?                      
         JNE   NO                                                               
         OC    GRPKCODE,GRPKCODE   PUB GROUP RECORD ?                           
         JZ    NO                  NO - DEFINITION RECORD                       
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PUB GROUP RECORD                                                   
*---------------------------------------------------------------------*         
INITPBG  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIPGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PUB GROUP PUB RECORDS                                                    
*---------------------------------------------------------------------*         
LOADPGP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LPGP01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPGKEY,GRPGKEY                                                  
         MVC   GRPGAGY,PRIALPHA                                                 
         MVI   GRPGTYP,GRPGBGQ                                                  
         MVC   GRPGMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPGP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         GOTO1 AACCLOAD,DMCB,VPRIPGPC,AINITPGP,AFILTPGP,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         CLI   IOKEY+3,GRPGBGQ                                                  
         JNE   LPGP03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPGP02                                                           
         J     YES                                                              
*                                                                               
LPGP03   AHI   R8,MEDTLNQ                                                       
         J     LPGP01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PUB GROUP PUB RECORD DATA                                              
*---------------------------------------------------------------------*         
UPDTPGP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIDIRQ     DIRECTORY ONLY RECORD                        
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPGMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTPGP                                                         
         JNE   YES                                                              
         GOTO1 AINITPGP                                                         
         GOTO1 AACCUPDT,DMCB,VPRIPGPC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PUB GROUP PUB RECORD AT R2                                   *         
*---------------------------------------------------------------------*         
FILTPGP  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPGAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPGTYP,GRPGBGQ     PUB GROUP PUB RECORD CODE ?                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PUB GROUP PUB RECORD                                               
*---------------------------------------------------------------------*         
INITPGP  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIPPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CUSTOM COLUMN RECORDS                                                    
*---------------------------------------------------------------------*         
LOADCCC  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LCCC01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PCOLRECD,R2                                                      
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PCOLKEY,PCOLKEY                                                  
         MVC   PCOLKAGY,PRIALPHA                                                
         MVI   PCOLKRCD,X'61'      RECORD CODE                                  
         MVI   PCOLKMED,C'A'       CUSTOM COL REC MEDIA ALWAYS "A"              
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCCC02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRICCCC,AINITCCC,AFILTCCC,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         CLI   IOKEY+3,X'61'       SAME RECORD CODE ?                           
         JNE   YES                 NO - DONE - ONLY ONE MEDIA ("A")             
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCCC02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CUSTOM COLUMN RECORD                                                   
*---------------------------------------------------------------------*         
UPDTCCC  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,02          VERSION 2 OR HIGHER ?                        
         JL    YES                 NO - DO NOT UPDATE THIS TABLE                
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PCOLRECD,R2                                                      
         LA    R1,PCOLKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTCCC                                                         
         JNE   YES                                                              
         GOTO1 AINITCCC                                                         
         GOTO1 AACCUPDT,DMCB,VPRICCCC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CUSTOM COLUMN RECORD AT R2                                   *         
*---------------------------------------------------------------------*         
FILTCCC  NTR1  BASE=*,LABEL=*                                                   
         USING PCOLRECD,R2                                                      
         CLC   PCOLKAGY,PRIALPHA   PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   PCOLKRCD,X'61'      CUSTOM COLUMN RECORD CODE ?                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE CUSTOM COLUMN RECORD                                               
*---------------------------------------------------------------------*         
INITCCC  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRICCDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PUB RECORDS                                                              
*---------------------------------------------------------------------*         
LOADPUB  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADINIT                                                      
         MVI   PRIPFLG,PUBKIDQ     X'81'                                        
*                                                                               
LPUB01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PUBREC,R2                                                        
         CLI   MEDTCODE,C'A'                                                    
         JE    LPUB05                                                           
         XC    PUBKEY,PUBKEY                                                    
         MVC   PUBKMED,MEDTCODE                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPUB02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LPUB05                                                           
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIPUBC,AINITPUB,AFILTPUB,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(1),0(R2)                                                   
         JNE   LPUB03               ALL DONE IF TYPE CHANGES                    
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPUB02                                                           
         J     LPUB05                                                           
*                                                                               
LPUB03   AHI   R8,MEDTLNQ                                                       
         J     LPUB01                                                           
*                                                                               
LPUB05   MVI   PRIPFLG,X'00'       BEFORE END LOADPUB SET FLAG TO 00            
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PUB RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTPUB  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PUBFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PUBREC,R2                                                        
         LA    R1,PUBKMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 FILTPUBR            DIFFERENT FILTER FOR RECOVERY FILE           
         JNE   YES                                                              
         GOTO1 AINITPUB                                                         
         GOTO1 AACCUPDT,DMCB,VPRIPUBC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PUB RECORD AT R2   (LOAD ONLY)                               *         
*    TESTS PUBFILE KEYS FOR PASSIVE POINTERS AND SKIPS THEM           *         
*---------------------------------------------------------------------*         
FILTPUB  NTR1  BASE=*,LABEL=*                                                   
         USING PUBREC,R2                                                        
         CLC   PUBKMED,0(R8)       SAME MEDIA AS IN MEDTAB?                     
         JNE   NO                                                               
         CLC   PUBKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   PUBKCOD,PUBKIDQ                                                  
         JNE   NO                                                               
         TM    PUBKEY+25,X'01'     SKIP IF PASSIVE POINTER                      
         JO    NO                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    YES                 YES                                          
         CLC   PUBKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PUB RECOVERY RECORD AT R2 (UPDATE ONLY)                      *         
*    THERE ARE NO PASSIVE POINTERS ON THE RECOVERY FILE               *         
*    (PUBKEY+25 IS THE RECORD LENGTH HERE)                            *         
*---------------------------------------------------------------------*         
FILTPUBR NTR1  BASE=*,LABEL=*                                                   
         USING PUBREC,R2                                                        
         CLI   PUBKCOD,PUBKIDQ                                                  
         JNE   NO                                                               
         TM    DXFLAGS,DXFAAGY     "ALL AGENCIES" ?                             
         JO    YES                 YES                                          
         CLC   PUBKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PUB RECORD                                                         
*---------------------------------------------------------------------*         
INITPUB  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIPBDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT GROUP DEFINITION RECORDS                                          
*---------------------------------------------------------------------*         
LOADCGD  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LCGD01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPKEY,GRPKEY                                                    
         MVC   GRPKAGY,PRIALPHA                                                 
         MVI   GRPKRCOD,GRPKCTYQ   X'34' REC CODE                               
         MVC   GRPKMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRICGDC,AINITCGD,AFILTCGD,(R8)                    
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,GRPKCTYQ X'34' REC CODE                                  
         JNE   LCGD03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCGD02                                                           
         J     YES                                                              
*                                                                               
LCGD03   AHI   R8,MEDTLNQ                                                       
         J     LCGD01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CLIENT GROUP DEFINITION RECORD DATA                                    
*---------------------------------------------------------------------*         
UPDTCGD  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPKMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTCGD                                                         
         JNE   YES                                                              
         GOTO1 AINITCGD                                                         
         GOTO1 AACCUPDT,DMCB,VPRICGDC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CLIENT GROUP DEFINITION RECORD AT R2                         *         
*---------------------------------------------------------------------*         
FILTCGD  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPKRCOD,GRPKCTYQ   PUB GROUP RECORD CODE (X'34') ?              
         JNE   NO                                                               
         OC    GRPKCODE,GRPKCODE   DEFINITION RECORD ?                          
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE CLIENT GROUP DEFINITION RECORD                                     
*---------------------------------------------------------------------*         
INITCGD  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRICDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT GROUP RECORDS                                                     
*---------------------------------------------------------------------*         
LOADCLG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LCLG01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPKEY,GRPKEY                                                    
         MVC   GRPKAGY,PRIALPHA                                                 
         MVI   GRPKRCOD,GRPKCTYQ                                                
         MVC   GRPKMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRICLGC,AINITCLG,AFILTCLG,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,GRPKCTYQ                                                 
         JNE   LCLG03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLG02                                                           
         J     YES                                                              
*                                                                               
LCLG03   AHI   R8,MEDTLNQ                                                       
         J     LCLG01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CLIENT GROUP RECORD DATA                                               
*---------------------------------------------------------------------*         
UPDTCLG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPKMED                                                       
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTCLG                                                         
         JNE   YES                                                              
         GOTO1 AINITCLG                                                         
         GOTO1 AACCUPDT,DMCB,VPRICLGC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CLIENT GROUP RECORD AT R2                                    *         
*---------------------------------------------------------------------*         
FILTCLG  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPKRCOD,GRPKCTYQ   CLIENT GROUP RECORD CODE ?                   
         JNE   NO                                                               
         OC    GRPKCODE,GRPKCODE   CLIENT GROUP RECORD ?                        
         JZ    NO                  NO - DEFINITION RECORD                       
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE CLIENT GROUP RECORD                                                
*---------------------------------------------------------------------*         
INITCLG  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRICGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT GROUP CLIENT RECORDS                                              
*---------------------------------------------------------------------*         
LOADCLC  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LCLC01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPGKEY,GRPGKEY                                                  
         MVC   GRPGAGY,PRIALPHA                                                 
         MVI   GRPGTYP,GRPGCGQ     X'3D' RECORD CODE                            
         MVC   GRPGMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLC02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         GOTO1 AACCLOAD,DMCB,VPRICLCC,AINITCLC,AFILTCLC,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         CLI   IOKEY+3,GRPGCGQ     X'3D' RECORD CODE                            
         JNE   LCLC03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLC02                                                           
         J     YES                                                              
*                                                                               
LCLC03   AHI   R8,MEDTLNQ                                                       
         J     LCLC01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CLIENT GROUP CLIENT RECORD                                             
*---------------------------------------------------------------------*         
UPDTCLC  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIDIRQ     DIRECTORY ONLY RECORD                        
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPGMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTCLC                                                         
         JNE   YES                                                              
         GOTO1 AINITCLC                                                         
         GOTO1 AACCUPDT,DMCB,VPRICLCC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CLIENT GROUP CLIENT RECORD AT R2                             *         
*---------------------------------------------------------------------*         
FILTCLC  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPGAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPGTYP,GRPGCGQ     CLT GROUP CLT REC CODE (X'3A') ?             
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE CLIENT GROUP CLIENT RECORD                                         
*---------------------------------------------------------------------*         
INITCLC  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRICLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT GROUP DEFINITION RECORDS                                         
*---------------------------------------------------------------------*         
LOADGDP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LGDP01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPKEY,GRPKEY                                                    
         MVC   GRPKAGY,PRIALPHA                                                 
         MVI   GRPKRCOD,GRPKPTYQ   X'35' PRD GRP CODE                           
         MVC   GRPKMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGDP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIGDPC,AINITGDP,AFILTGDP,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,GRPKPTYQ     X'35' PRD GRP CODE                          
         JNE   LGDP03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGDP02                                                           
         J     YES                                                              
*                                                                               
LGDP03   AHI   R8,MEDTLNQ                                                       
         J     LGDP01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP DEFINITION RECORD                                        
*---------------------------------------------------------------------*         
UPDTGDP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPKMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTGDP                                                         
         JNE   YES                                                              
         GOTO1 AINITGDP                                                         
         GOTO1 AACCUPDT,DMCB,VPRIGDPC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PRODUCT GROUP DEFINITION RECORD AT R2                        *         
*---------------------------------------------------------------------*         
FILTGDP  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPKRCOD,GRPKPTYQ   X'35' PRD GRP CODE                           
         JNE   NO                                                               
         OC    GRPKCODE,GRPKCODE   DEFINITION RECORD ?                          
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP DEFINITION RECORD                                    
*---------------------------------------------------------------------*         
INITGDP  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIGDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT GROUP RECORDS                                                    
*---------------------------------------------------------------------*         
LOADGGP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LGGP01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPKEY,GRPKEY                                                    
         MVC   GRPKAGY,PRIALPHA                                                 
         MVI   GRPKRCOD,GRPKPTYQ   X'35' PRD GROUP RECORD                       
         MVC   GRPKMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGGP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIGGPC,AINITGGP,AFILTGGP,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,GRPKPTYQ    X'35' PRD GROUP RECORD                       
         JNE   LGGP03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGGP02                                                           
         J     YES                                                              
*                                                                               
LGGP03   AHI   R8,MEDTLNQ                                                       
         J     LGGP01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP RECORD                                                   
*---------------------------------------------------------------------*         
UPDTGGP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPKMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTGGP                                                         
         JNE   YES                                                              
         GOTO1 AINITGGP                                                         
         GOTO1 AACCUPDT,DMCB,VPRIGGPC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PRODUCT GROUP RECORD AT R2                                   *         
*---------------------------------------------------------------------*         
FILTGGP  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPKRCOD,GRPKPTYQ   X'35' PRD GROUP RECORD?                      
         JNE   NO                                                               
         OC    GRPKCODE,GRPKCODE   PRD GROUP RECORD ?                           
         JZ    NO                  NO - DEFINITION RECORD                       
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP RECORD                                               
*---------------------------------------------------------------------*         
INITGGP  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIRGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT GROUP PRODUCT RECORDS                                            
*---------------------------------------------------------------------*         
LOADGPP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LGPP01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    GRPGKEY,GRPGKEY                                                  
         MVC   GRPGAGY,PRIALPHA                                                 
         MVI   GRPGTYP,GRPGPGQ     X'3E' PRD GROUP PRD REC                      
         MVC   GRPGMED,MEDTCODE                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGPP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         GOTO1 AACCLOAD,DMCB,VPRIGPPC,AINITGPP,AFILTGPP,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF AGENCY CHANGES                  
*                                                                               
         CLI   IOKEY+3,GRPGPGQ     X'3E' PRD GROUP PRD REC                      
         JNE   LGPP03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGPP02                                                           
         J     YES                                                              
*                                                                               
LGPP03   AHI   R8,MEDTLNQ                                                       
         J     LGPP01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP PRODUCT RECORD DATA                                      
*---------------------------------------------------------------------*         
UPDTGPP  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIDIRQ     DIRECTORY ONLY RECORD                        
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GRPRECD,R2                                                       
         LA    R1,GRPGMED          PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTGPP                                                         
         JNE   YES                                                              
         GOTO1 AINITGPP                                                         
         GOTO1 AACCUPDT,DMCB,VPRIGPPC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PRODUCT GROUP PRODUCT RECORD AT R2                           *         
*---------------------------------------------------------------------*         
FILTGPP  NTR1  BASE=*,LABEL=*                                                   
         USING GRPRECD,R2                                                       
         CLC   GRPGAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   GRPGTYP,GRPGPGQ     X'3E' PRD GROUP PRD REC ?                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP PRODUCT RECORD                                       
*---------------------------------------------------------------------*         
INITGPP  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIGPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD REGION RECORDS                                                           
*---------------------------------------------------------------------*         
LOADREG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LREG01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PREGRECD,R2                                                      
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PREGKEY,PREGKEY                                                  
         MVC   PREGKAGY,PRIALPHA                                                
         MVI   PREGKRCD,PREGKIDQ                                                
         MVC   PREGKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIREGC,AINITREG,AFILTREG,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PREGKIDQ                                                 
         JNE   LREG03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREG02                                                           
         J     YES                                                              
*                                                                               
LREG03   AHI   R8,MEDTLNQ                                                       
         J     LREG01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE REGION RECORD DATA                                                     
*---------------------------------------------------------------------*         
UPDTREG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PREGRECD,R2                                                      
         LA    R1,PREGKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTREG                                                         
         JNE   YES                                                              
         GOTO1 AINITREG                                                         
         GOTO1 AACCUPDT,DMCB,VPRIREGC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER REGION RECORD AT R2                                          *         
*---------------------------------------------------------------------*         
FILTREG  NTR1  BASE=*,LABEL=*                                                   
         USING PREGRECD,R2                                                      
         CLC   PREGKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PREGKRCD,PREGKIDQ    X'04' RECORD CODE                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE REGION RECORD                                                      
*---------------------------------------------------------------------*         
INITREG  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIRGDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD DISYRICT RECORDS                                                         
*---------------------------------------------------------------------*         
LOADDST  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LDST01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PDSTRECD,R2                                                      
         CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
         XC    PDSTKEY,PDSTKEY                                                  
         MVC   PDSTKAGY,PRIALPHA                                                
         MVI   PDSTKRCD,PDSTKIDQ                                                
         MVC   PDSTKMED,MEDTCODE                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         MVC   PRIADDR,27(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VPRIDSTC,AINITDST,AFILTDST,MEDTABD                 
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                  ALL DONE IF TYPE CHANGES                    
*                                                                               
         CLI   IOKEY+3,PDSTKIDQ                                                 
         JNE   LDST03                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDST02                                                           
         J     YES                                                              
*                                                                               
LDST03   AHI   R8,MEDTLNQ                                                       
         J     LDST01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE DISTRICT RECORD DATA                                                   
*---------------------------------------------------------------------*         
UPDTDST  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PDSTRECD,R2                                                      
         LA    R1,PDSTKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTDST                                                         
         JNE   YES                                                              
         GOTO1 AINITDST                                                         
         GOTO1 AACCUPDT,DMCB,VPRIDSTC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER DISTRICT RECORD AT R2                                        *         
*---------------------------------------------------------------------*         
FILTDST  NTR1  BASE=*,LABEL=*                                                   
         USING PDSTRECD,R2                                                      
         CLC   PDSTKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PDSTKRCD,PDSTKIDQ    X'05' RECORD CODE                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE DISTRICT RECORD                                                    
*---------------------------------------------------------------------*         
INITDST  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIDSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PUB DISTRICT ASSIGNMENT RECORDS                                          
*---------------------------------------------------------------------*         
LOADPDA  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    LPDA99              NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
         MVI   PRIPFLG,LTLKIDQ     X'85'                                        
*                                                                               
LPDA01   CLI   MEDTCODE,C'A'                                                    
         JE    LPDA99                                                           
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING LTLRECD,R2                                                       
         XC    LTLKEY,LTLKEY                                                    
         MVC   LTLKAGY,PRIALPHA                                                 
         MVC   LTLKMED,MEDTCODE                                                 
         MVI   LTLKCOD,LTLKIDQ     X'85'                                        
*                                                                               
         L     R2,DXARECB                                                       
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                  ERROR ON READ HIGH                           
***                                                                             
LPDA02   DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LPDA99                                                           
*                                                                               
         CLC   IOKEY(1),0(R2)      SAME MEDIA ?                                 
         JNE   LPDA09                                                           
*                                                                               
         GOTO1 AFILTPDA                                                         
         JNE   LPDA08                                                           
*                                                                               
         MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         MVI   ELCODE,X'71'                                                     
         LR    R4,R2               POINT R4 TO BEGINNING OF LTL REC             
         BRAS  RE,GETEL                                                         
         JNE   LPDA08              NO PUB DISTRICT ELEM - NEXT REC              
*                                                                               
         GOTO1 AINITPDA            INITIALISE EXTRACT BUFFER                    
*                                  CALL TO CREATE RECORD(S)                     
         GOTO1 VPRIPDAC,DMCB,DXAXREC,(R2),(1,0),(1,R6),MEDTABD                  
LPDA04   DS    0H                  CALL TO OUTPUT RECORD(S)                     
         GOTO1 VPRIPDAC,DMCB,DXAXREC,(R2),(2,0),(R6),MEDTABD                    
         CLI   8(R1),X'FF'                                                      
         JE    LPDA08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPDA08              DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LPDA06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LPDA06   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
LPDA07   DS    0H                                                               
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LPDA04              SEE IF MORE RECORDS TO OUTPUT                
*                                                                               
LPDA08   DS    0H                  READ NEXT RECORDS                            
*                                                                               
         MVC   IOKEY(L'LTLKEY),0(R2) READ NEXT RECORD - SEQUENTIAL              
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LPDA99                                                           
*                                                                               
         CLC   IOKEY(1),0(R2)      SAME MEDIA ?                                 
         JNE   LPDA09                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,DMRSEQ),PUBDIR,IOKEY,(R2),DMWORK                
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPDA02                                                           
         J     LPDA99                                                           
*                                                                               
LPDA09   AHI   R8,MEDTLNQ                                                       
         J     LPDA01                                                           
*                                                                               
LPDA99   DS    0H                                                               
         MVI   PRIPFLG,X'00'       USED IN READHI AND GETIT                     
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDTPDA - UPDATE PDA (PUB DISTRICT ASSIGNMENTS) RECORD                        
*---------------------------------------------------------------------*         
UPDTPDA  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,03          VERSION 3 OR HIGHER ?                        
         JL    YES                 NO - DO NOT PROCESS THIS TABLE               
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PUBFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING LTLREC,R2                                                        
         LA    R1,LTLKMED                                                       
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTPDA                                                         
         JNE   YES                                                              
*                                                                               
         MVI   ELCODE,X'71'                                                     
         LR    R4,R2               POINT R4 TO BEGINNING OF LTL REC             
         BRAS  RE,GETEL                                                         
         JNE   YES                 NO PUB DISTRICT ELEM                         
*                                                                               
         GOTO1 AINITPDA                                                         
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)                                                      
         AR    R1,R2                                                            
         MVI   0(R1),X'00'      INSERT X'00' AT THE END OF RECORD               
*                                                                               
         MVI   ROUTFLG,3        TELL PTXROUTS THIS IS UPDATE NOT LOAD           
         CLI   RRECTY,3            ADD RECORD ?                                 
         BNE   *+8                 NO                                           
         MVI   ROUTFLG,1           YES - TREAT LIKE LOAD IN PTXROUTS            
*                               CALL TO CREATE RECORD(S)                        
         GOTO1 VPRIPDAC,DMCB,DXAXREC,(R2),(ROUTFLG,0),(R6),(R8)                 
*                                                                               
UPDTPDA5 DS    0H               CALL TO OUTPUT RECORD(S)                        
         GOTO1 VPRIPDAC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    UPDTPDA9            NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPDTPDA9            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UPDTPDA6            DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UPDTPDA6 DS    0H                                                               
*                                                                               
UPDTPDA7 DS    0H                                                               
*                                                                               
UPDTPDA8 DS    0H                                                               
*                                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     UPDTPDA5                                                         
*                                                                               
UPDTPDA9 DS    0H                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PUB DISTRICT ASSIGNMENT RECORD AT R2                                   
*---------------------------------------------------------------------*         
FILTPDA  NTR1  BASE=*,LABEL=*                                                   
         USING LTLRECD,R2                                                       
         CLC   LTLKAGY,PRIALPHA    PRIALPHA OK?                                 
         JNE   NO                                                               
         CLI   LTLKCOD,LTLKIDQ                                                  
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PUB DISTRICT ASSIGNMENT RECORD                                     
*---------------------------------------------------------------------*         
INITPDA  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIDPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ADDITIONAL CHARGE RECORDS  (NOTE - ONLY 1 RECORD PER MEDIA)              
*---------------------------------------------------------------------*         
LOADACG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,05          VERSION 5 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LACG01   CLI   MEDTCODE,C'A'                                                    
         JNE   LACG010                                                          
         J     YES                                                              
*                                                                               
LACG010  DS    0H                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PSPLRECD,R2                                                      
         XC    PSPLKEY,PSPLKEY                                                  
*                                                                               
         MVC   PSPLKAGY,PRIALPHA                                                
         MVC   PSPLKMED,MEDTCODE                                                
         MVI   PSPLKRCD,PSPLKIDQ    X'60'                                       
*                                                                               
         L     R2,DXARECB                                                       
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                  ERROR ON READ HIGH                           
*                                                                               
LACG02   DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILTACG                                                         
         JNE   LACG09              NEXT MEDIA                                   
*                                                                               
         MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         LR    R4,R2               POINT R4 TO START OF ADDL CHG REC            
         BRAS  RE,GETEL                                                         
         JNE   LACG09              NO ADDITIONAL CHARGES - NEXT MEDIA           
*                                                                               
         GOTO1 AINITACG            INITIALISE EXTRACT BUFFER                    
*                                  CALL TO CREATE RECORD(S)                     
         GOTO1 VPRIACGC,DMCB,DXAXREC,(R2),(1,0),(1,R6),MEDTABD                  
LACG04   DS    0H                  CALL TO OUTPUT RECORD(S)                     
         GOTO1 VPRIACGC,DMCB,DXAXREC,(R2),(2,0),(R6),MEDTABD                    
         CLI   8(R1),X'FF'                                                      
         JE    LACG09              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LACG09              DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LACG06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LACG06   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
LACG07   DS    0H                                                               
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LACG04                                                           
*                                                                               
LACG09   AHI   R8,MEDTLNQ                                                       
         J     LACG01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDTACG - UPDATE ACG (ADDITIONAL CHARGE) RECORD                               
*---------------------------------------------------------------------*         
UPDTACG  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,05          VERSION 5 OR HIGHER ?                        
         JL    YES                 NO - DO NOT PROCESS THIS TABLE               
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PSPLRECD,R2                                                      
         LA    R1,PSPLKMED         PASS R1 TO GETMED                            
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTACG                                                         
         JNE   YES                                                              
*                                                                               
         MVI   ELCODE,X'10'                                                     
         LR    R4,R2               POINT R4 TO START OF ADDL CHG REC            
         BRAS  RE,GETEL                                                         
         JNE   YES                 NO ADDITIONAL CHARGES                        
*                                                                               
         GOTO1 AINITACG                                                         
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)                                                      
         AR    R1,R2                                                            
         MVI   0(R1),X'00'      INSERT X'00' AT THE END OF RECORD               
*                                                                               
         MVI   ROUTFLG,3        TELL PTXROUTS THIS IS UPDATE NOT LOAD           
         CLI   RRECTY,3            ADD RECORD ?                                 
         BNE   *+8                 NO                                           
         MVI   ROUTFLG,1           YES - TREAT LIKE LOAD IN PTXROUTS            
*                               CALL TO CREATE RECORD(S)                        
         GOTO1 VPRIACGC,DMCB,DXAXREC,(R2),(ROUTFLG,0),(R6),(R8)                 
*                                                                               
UPDTACG5 DS    0H               CALL TO OUTPUT RECORD(S)                        
         GOTO1 VPRIACGC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    YES                 NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UPDTACG6            DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UPDTACG6 DS    0H                                                               
*                                                                               
UPDTACG7 DS    0H                                                               
*                                                                               
UPDTACG8 DS    0H                                                               
*                                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     UPDTACG5                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER ADDITIONAL CHARGE RECORD AT R2                                         
*---------------------------------------------------------------------*         
FILTACG  NTR1  BASE=*,LABEL=*                                                   
         USING PSPLRECD,R2                                                      
         CLC   PSPLKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PSPLKRCD,PSPLKIDQ                                                
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* INITIALISE ADDITIONAL CHARGE RECORD                                           
*---------------------------------------------------------------------*         
INITACG  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIACDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* LOAD USER COMMENT (UCOMM) RECORDS                                             
*---------------------------------------------------------------------*         
LOADUCM  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,06          VERSION 6 OR HIGHER ?                        
         JL    YES                 NO - DO NOT LOAD THIS TABLE                  
*                                                                               
         BRAS  RE,LOADINIT                                                      
*                                                                               
LUCM01   CLI   MEDTCODE,C'A'                                                    
         JE    YES                                                              
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PUCMREC,R2                                                       
         XC    PUCMKEY,PUCMKEY                                                  
*                                                                               
         MVC   PUCMKAGY,PRIALPHA                                                
         MVC   PUCMKMED,MEDTCODE                                                
         MVI   PUCMKRCD,X'12'                                                   
*                                                                               
         L     R2,DXARECB                                                       
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                  ERROR ON READ HIGH                           
*                                                                               
LUCM02   DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTUCM                                                         
         JNE   LUCM09              NEXT MEDIA                                   
*                                                                               
         MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         OC    PUCMKPRD,PUCMKPRD   CLIENT UCOMM ?                               
         JNZ   LUCM03              NO                                           
*                                                                               
         BRAS  RE,UCOMDEF          STORE THE CLIENT UCOMM REC                   
         J     LUCM08              READ NEXT RECORD                             
*                                                                               
LUCM03   DS    0H                                                               
*                                                                               
         GOTO1 AINITUCM            INITIALISE EXTRACT BUFFER                    
*                                                                               
*                                  CALL TO CREATE RECORD(S)                     
         GOTO1 VPRIUCMC,DMCB,DXAXREC,(R2),(1,0),(1,R6),(R8),UCOMCLT             
LUCM04   DS    0H                  CALL TO OUTPUT RECORD(S)                     
         GOTO1 VPRIUCMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8),UCOMCLT               
         CLI   8(R1),X'FF'                                                      
         JE    LUCM08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LUCM08              DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LUCM06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LUCM06   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LUCM04              SEE IF MORE RECORDS TO OUTPUT                
*                                                                               
LUCM08   DS    0H                  READ NEXT RECORDS                            
*                                                                               
         MVC   IOKEY(L'PUCMKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTUCM                                                         
         JNE   LUCM09              NEXT MEDIA                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,DMRSEQ),PRTDIR,IOKEY,(R2),DMWORK                
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUCM02                                                           
         J     YES                                                              
*                                                                               
LUCM09   AHI   R8,MEDTLNQ                                                       
         J     LUCM01                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDTUCM - UPDATE USER COMMENT (UCOMM) RECORDS                                 
*---------------------------------------------------------------------*         
UPDTUCM  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,06          VERSION 6 OR HIGHER ?                        
         JL    YES                 NO - DO NOT PROCESS THIS TABLE               
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RECVHDR,PRIFILQ                                                  
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PUCMREC,R2                                                       
         LA    R1,PUCMKMED                                                      
         BRAS  RE,GETMED           R8=A(MEDIA TABLE ENTRY)                      
         GOTO1 AFILTUCM                                                         
         JNE   YES                                                              
*                                                                               
         OC    PUCMKPRD,PUCMKPRD   CLIENT UCOMM REC ?                           
         JNZ   UPUCM03             NO                                           
***********************************************************************         
* FOLLOWING ROUTINE TO HANDLE CHANGES TO UCOMM CLIENT RECORD COMMENTED          
* OUT IN SEPTEMBER, 2010 - ONLY THE W/E LOAD PROCESS WILL UPDATE UCOMM          
* NON-CLIENT RECORDS UNLESS THEIR "FIELD DATA" IS CHANGED                       
*                                                                               
*        BRAS  RE,UPUCMCLT           SPECIAL PROCESSING                         
***********************************************************************         
*                                                                               
         J     YES                 SKIP THE RCV FILE CLT UCOMM REC              
*                                                                               
UPUCM03  DS    0H                                                               
*                                                                               
         BRAS  RE,UCOMDEF          STORE THE CLIENT UCOMM REC                   
*                                                                               
         GOTO1 AINITUCM                                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)                                                      
         AR    R1,R2                                                            
         MVI   0(R1),X'00'      INSERT X'00' AT THE END OF RECORD               
*                                                                               
         MVI   ROUTFLG,3        TELL PTXROUTS THIS IS UPDATE NOT LOAD           
         CLI   RRECTY,3            ADD RECORD ?                                 
         BNE   *+8                 NO                                           
         MVI   ROUTFLG,1           YES - TREAT LIKE LOAD IN PTXROUTS            
*                               CALL TO CREATE RECORD(S)                        
         GOTO1 VPRIUCMC,DMCB,DXAXREC,(R2),(ROUTFLG,0),(R6),(R8),UCOMCLT         
*                                                                               
UPUCM05  DS    0H               CALL TO OUTPUT RECORD(S)                        
         GOTO1 VPRIUCMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8),UCOMCLT               
         CLI   8(R1),X'FF'                                                      
         JE    UPUCM09             NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPUCM09             DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UPUCM06             DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UPUCM06  DS    0H                                                               
*                                                                               
UPUCM07  DS    0H                                                               
*                                                                               
UPUCM08  DS    0H                                                               
*                                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     UPUCM05                                                          
*                                                                               
UPUCM09  DS    0H                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*---------------------------------------------------------------------*         
* FILTER  USER COMMENT (UCOMM) RECORD AT R2                                     
*---------------------------------------------------------------------*         
FILTUCM  NTR1  BASE=*,LABEL=*                                                   
         USING PUCMREC,R2                                                       
         CLC   PUCMKAGY,PRIALPHA    PRIALPHA OK?                                
         JNE   NO                                                               
         CLI   PUCMKRCD,X'12'                                                   
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* INITIALISE USER COMMENT (UCOMM) RECORD                                        
*---------------------------------------------------------------------*         
INITUCM  NTR1  BASE=*,LABEL=*                                                   
*        LA    R1,PRIUCDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* UCOMDEF - SAVE UCOMM CLIENT RECORD IN UCOMCLT FOR USE IN PTXROUTS             
*           IT CONTAINS THE FIELD NAMES FOR THE VARIOUS USER COMMENTS           
* R2 EXPECTED TO ADDRESS UCOM RECORD                                            
***********************************************************************         
UCOMDEF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   0(25,R2),UCOMCLT    DO WE ALREADY HAVE UCOMM CLT REC ?           
         BE    UCOMDQX             YES - DONE                                   
*                                                                               
         LA    RE,UCOMCLT                                                       
         LHI   RF,500                                                           
         XCEFL                                                                  
*                                                                               
         USING PUCMKEY,R2                                                       
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         MVC   IOKEYSAV(25),0(R2)  SAVE KEY FOR READ SEQ RESTORE                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(PUCMKPRD-PUCMKEY),0(R2)    CLT-LEVEL KEY                     
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(25),KEY                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),PRTDIR,KEY,KEY                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',GETREC),PRTFIL,KEY+27,UCOMCLT,     X        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'02'         RECORD DELETED - IT IS OK                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* NOW HAVE CLIENT-LEVEL UCOM RECORD IN UCOMCLT                                  
*                                                                               
*                                                                               
* RESTORE READ SEQUENCE                                                         
*                                                                               
         CLI   DXMODE,DXUPDTQ                                                   
         BE    UCOMDQX             NO RESTORE ON UPDATE                         
         OC    PUCMKPRD,PUCMKPRD                                                
         BZ    UCOMDQX             WE'RE ON CLT-LEVEL RECORD ALREADY            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),IOKEYSAV                                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                SEQ RESTORE FAILED                           
*                                                                               
         CLC   KEY(25),IOKEYSAV                                                 
         BE    *+6                                                              
         DC    H'0'                SEQ RESTORE FAILED                           
*                                                                               
UCOMDQX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* UPUCMCLT                                                                      
* SPECIAL HANDLING FOR RECOVERY FILE CHANGE TO UCOMM CLIENT RECORD              
* DELETE (KILL) AND RE-ADD ALL UCOMM RECORDS FOR THIS AGY/MED/CLT               
*                                                                               
***********************************************************************         
UPUCMCLT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PUCMREC,R2                                                       
         XC    PUCMKEY,PUCMKEY                                                  
         MVC   PUCMKEY,UCOMCLT     UCOMCLT HAS UCOMM CLIENT RECORD              
*                                                                               
         L     R2,DXARECB                                                       
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   UPUCLXT             ERROR ON READ HIGH                           
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    UPUCLXT                                                          
*                                                                               
UPUCL02  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(0,DMRSEQ),PRTDIR,IOKEY,(R2),DMWORK                
         CLC   IOKEY(7),0(R2)      AGY/MED/RC/CLT                               
         JNE   UPUCLXT             DONE                                         
*                                                                               
         MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
***********************         *****************                               
* INSERT X'00' AT THE END OF RECORD                                             
***********************         *****************                               
         SR    R1,R1                                                            
         ICM   R1,3,25(R2)                                                      
         AR    R1,R2                                                            
         MVI   0(R1),X'00'                                                      
***********************         *****************                               
*                                                                               
*                               CALL TO CREATE RECORD(S)                        
         GOTO1 VPRIUCMC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8),UCOMCLT               
UPUCL05  DS    0H               CALL TO OUTPUT RECORD(S)                        
         GOTO1 VPRIUCMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8),UCOMCLT               
         CLI   8(R1),X'FF'                                                      
         JE    UPUCL02             NO MORE RECORDS - NEXT                       
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPUCL02             DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UPUCL09             DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UPUCL09  DS    0H                                                               
*                                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   UPUCLXT                                                          
         J     UPUCL05             SEE IF MORE RECORDS TO OUTPUT                
*                                                                               
UPUCLXT  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         L     R8,12(R1)                                                        
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   DS    0H                                                               
         CLI   IOKEY+3,GRPGCGQ     X'3D' CLT GRP CLT DIR ONLY REC ?             
         JE    ALOA02I             YES - SKIP GETREC                            
*                                                                               
         CLI   IOKEY+3,GRPGPGQ     X'3E' PRD GRP PRD DIR ONLY REC ?             
         JE    ALOA02I             YES - SKIP GETREC                            
*                                                                               
         CLI   IOKEY+3,GRPGBGQ     X'3F' PUB GRP PUB DIR ONLY REC ?             
         JE    ALOA02I             YES - SKIP GETREC                            
*                                                                               
         TM    PTXPFLG1,NORDSEQ                                                 
         JO    ALOA02I                                                          
*                                                                               
         CLI   PPRDKRCD-PPRDKEY(R2),X'06' PRODUCT?                              
         BNE   ALOA02B                                                          
         CLC   SVAMCLT,0(R2)       CLIENT CHANGED?                              
         BE    ALOA02B                                                          
         MVC   SVAMCLT,0(R2)                                                    
         XC    SVAAABFM,SVAAABFM   XC PRODUCT AAA'S BILL FORMULA                
*                                                                               
ALOA02B  DS    0H                                                               
         CLI   IOKEY+3,X'18'       PBUDKRCD, BUDGET RECORD                      
         BNE   ALOA02G                                                          
*                                                                               
         OI    PTXPFLG1,BLDMOSTQ   BUILD MOS TABLE                              
         CLC   SVAMCLT,0(R2)                                                    
         BE    ALOA02G                                                          
         MVC   SVAMCLT,0(R2)                                                    
         BRAS  RE,GETCPROF                                                      
         MVC   BUDTYPE,SVCPROF+14  BUDGET TYPE                                  
*                                                                               
ALOA02G  DS    0H                                                               
         GOTO1 AGETIT              ****  GET RECORD  ****                       
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
ALOA02I  DS    0H                                                               
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                                                               
*                                  CALL RECORD EXTRACT ROUTINE                  
ALOA03   GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8),PTXPARM                       
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
*                                                                               
         TM    PTXPFLG1,NOWRITEQ                                                
         BZ    *+12                                                             
         NI    PTXPFLG1,X'FF'-NOWRITEQ                                          
         J     ALOA06                                                           
*                                                                               
         CLI   PPRDKRCD-PPRDKEY(R2),X'06' PRODUCT?                              
         BNE   ALOA04                                                           
         L     RF,DXAXREC                                                       
         CLC   =C'AAA',PRIPDPRD-PRIPDD(RF)                                      
         BNE   ALOA04                                                           
         MVC   SVAAABFM,PRIPDBFM-PRIPDD(RF) SAVE AAA'S BILL FORMULA             
*                                                                               
ALOA04   DS    0H                                                               
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ALOA05              PUT UNCONVERTED RECORD ONLY                  
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         J     ALOA06              REPEAT ROUTINE                               
*                                                                               
ALOA05   DS    0H                                                               
*                                  PUT UNCONVERTED RECORD TO FILE               
         GOTO1 DXPUT,DMCB,DXAXREC,(R7)                                          
*                                                                               
ALOA06   DS    0H                                                               
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         CLI   PRIPFLG,PUBKIDQ     FOR PUB'81' WE NEED ONLY ONE READHI          
         JE    ALOA07              SKIP READHI                                  
*                                                                               
         MVC   IOKEY(L'PAGYKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         CLI   PRIPFLG,PUBKIDQ     JUMP TO READ NOT PUB DIR                     
         JNE   ALOA08                                                           
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
ALOA07   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,PUBDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
ALOA08   DS    0H                                                               
         TM    PTXPFLG1,NORDSEQ                                                 
         JO    YES                                                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,PRTDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
REPMED   DC    C' '           STORAGE FOR ONE BYTE MEDIA                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN UPDATE MODE                *         
* R2 = A(ACCOUNT RECORD BUFFER)                                       *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         L     R8,8(R1)                                                         
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8),PTXPARM                       
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPDL04              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   UPDL02                                                           
*                                                                               
* MULTIPLE LINES EXTRACTED FOR BUDGET RECORDS                                   
* DO NOT COMPARE COPY/CHANGE                                                    
         L     RF,DXAXREC                                                       
         L     RE,=A(PRBDQ)                                                     
         CLC   0(2,RE),(PRIBDTYP-PRIBDD)(RF)                                    
         BE    UPDL02                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(L'COPYBUFF)                                                
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6),(R8)    BUILD COPY REC            
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
         LH    R1,=AL2(PRIMDAGY-PRIMDD) DISP TO PRIALPHA                        
         AR    R0,R1               BUMP TO PRIALPHA CODE ALL RECORDS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
*                                  DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
UPDL02   DS    0H                                                               
         GOTO1 VPTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UPDL04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRTFILE INITIALISATION                                              *         
*                                                                               
* WILL EXTRACT ONE BYTE FROM AGENCY RECORD IN ORDER TO DEAL WITH                
* PUB NUMBERS IN SOME OTHER RECORDS AND STORE THEM IN PROFTAB TABLE             
***********************************************************************         
PRIINIT  NTR1  BASE=*,LABEL=*                                                   
KAP      USING PAGYKEY,IOKEY                                                    
         XC    ONLYB,ONLYB                                                      
         LARL  R8,MEDTAB                                                        
INIT10   CLI   MEDTCODE,EOT            EOT                                      
         JE    YES                                                              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   KAP.PAGYKAGY,PRIALPHA   AGENCY ALPHA                             
         MVC   KAP.PAGYKMED,MEDTCODE   MEDIA                                    
         MVI   KAP.PAGYKRCD,X'01'      RECORD CODE                              
         DROP  KAP                                                              
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'PAGYKEY),0(R2)                                           
         JE    INIT20                                                           
         AHI   R8,MEDTLNQ          BUMP TO NEXT ENTRY                           
         J     INIT10                                                           
*                                                                               
INIT20   MVC   PRIADDR,27(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R        USING PAGYREC,R2                                                       
         MVC   MEDTPROF,R.PAGYPROF+12                                           
         CLI   R.PAGYMED,C'I'                                                   
         BNE   INIT30                                                           
         MVC   MEDTNAME,=C'INTERACTIVE'                                         
         B     INIT40                                                           
INIT30   MVC   MEDTNAME(L'R.PAGYMED),R.PAGYMED                                  
INIT40   AHI   R8,MEDTLNQ                                                       
         J     INIT10                                                           
         J     YES                                                              
         DROP  R                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*GETEL                                                                          
         GETEL R4,33,ELCODE                                                     
*                                                                               
*---------------------------------------------------------------------*         
* CODE IS RELATIVE. DOES USE RA FOR ADDRESS (WORKING GLOBAL AREA)               
*                                                                               
* GET MEDIA ENTRY                                                               
* R1 = MEDIA CODE TO FIND                                                       
*---------------------------------------------------------------------*         
GETMED   LARL  R8,MEDTAB                                                        
GETMED2  CLI   MEDTCODE,EOT                                                     
         JE    *+2                                                              
         CLC   MEDTCODE,0(R1)                                                   
         BER   RE                                                               
         AHI   R8,MEDTLNQ                                                       
         J     GETMED2                                                          
*                                                                               
LOADINIT DS    0H                                                               
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   ONLYB,X'00'         RE-SET, TO AVOID READING DELETES             
         MVI   PRIPFLG,00                                                       
*                                                                               
         MVI   PTXPFLG1,0                                                       
         MVI   ELCOUNT,0                                                        
*                                                                               
         LARL  R8,MEDTAB                                                        
         BR    RE                                                               
*                                                                               
***********************************************************************         
* OBTAIN BILL FORMULA FOR ESTIMATE'S PRODUCT                                    
***********************************************************************         
GETPRBFM NTR1  BASE=*,LABEL=*,WORK=(R8,4)                                       
         MVC   0(L'PESTKEY,R8),0(R2) SAVE KEY IN WORKING STORAGE                
         MVC   SVPRDBFM,SPACES                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PPRDKEY,R4                                                       
         MVC   KEY(PESTKEST-PESTKEY),0(R2) A/M/07/CLT/PRD FROM EST KEY          
         MVI   PPRDKRCD,X'06'      PRODUCT RECORD                               
         GOTO1 =A(GETBFM),DMCB,KEY,SVPRDBFM                                     
         BE    GETPRBFM5                                                        
*                                                                               
         MVC   PPRDKPRD,=C'AAA'                                                 
         GOTO1 =A(GETBFM),DMCB,KEY,SVPRDBFM                                     
* RESTORE READ SEQUENCE                                                         
GETPRBFM5 DS   0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PESTKEY),0(R8)                                             
         GOTO1 VDATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                              
         CLC   KEY(L'PESTKEY),0(R8)                                             
         JE    YES                                                              
         DC    H'0'                READ SEQUENCE RESTORE FAILED                 
         LTORG                                                                  
*                                                                               
* P1 - A(PRODUCT KEY)                                                           
* P2 - A(BILL FORMULA OUTPUT AREA, 18 BYTES)                                    
GETBFM   NTR1  BASE=*,LABEL=*,WORK=(R8,4)                                       
         L     R4,0(R1)            PRODUCT KEY                                  
         L     R3,4(R1)            OUTPUT                                       
         XC    0(L'PRIPDBFM,R3),0(R3) INITIALIZE OUTPUT AREA                    
         MVC   0(L'PPRDKEY,R8),0(R4) SAVE PRD KEY IN WORKING STORAGE            
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,PRTDIR,(R4),IO                              
         CLC   IO(L'PPRDKEY),0(R8)                                              
         JNE   NEQXIT                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,GETREC,PRTFIL,IO+27,IO,DMWORK                      
         CLI   8(R1),0                                                          
         JNE   NEQXIT                                                           
*                                                                               
         MVI   ELCODE,X'06'        PPRDELEM                                     
         LA    R4,IO               POINT R4 TO BEGINNING OF REC                 
         BRAS  RE,GETEL                                                         
         JNE   NEQXIT                                                           
*                                                                               
         USING PPRDELEM,R4                                                      
         GOTO1 =V(DISBFM),DMCB,PPRDBILP,(R3)                                    
         DROP  R4                                                               
         CLC   0(L'PRIPDBFM,R3),SPACES OUTPUT AREA                              
         JH    EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
***********************************************************************         
* PTXBRCID                                                                      
***********************************************************************         
         PRINT OFF                                                              
PTXRCIDT DS    0C                                                               
       ++INCLUDE PTXBRCID                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*  MEDIA TABLE                                                                  
***********************************************************************         
MEDTAB   DS    0H                                                               
         DC    C'B'                                                             
         DS    C                                                                
         DS    CL11                   'MOBILE'                                  
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'D'                                                             
         DS    C                                                                
         DS    CL11                   'DIGITAL AUDIO'                           
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'I'                                                             
         DS    C                                                                
         DS    CL11                   'INTERACTIVE'                             
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'L'                                                             
         DS    C                                                                
         DS    CL11                   'SOCIAL'                                  
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'M'                                                             
         DS    C                                                                
         DS    CL11                   'MAGAZINES'                               
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'N'                                                             
         DS    C                                                                
         DS    CL11                   'NEWSPAPERS'                              
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'O'                                                             
         DS    C                                                                
         DS    CL11                   'OUTDOOR'                                 
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'S'                                                             
         DS    C                                                                
         DS    CL11                   'SUPPLEMENT'                              
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'T'                                                             
         DS    C                                                                
         DS    CL11                   'TRADE'                                   
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'V'                                                             
         DS    C                                                                
         DS    CL11                   'NATL VIDEO'     NATIONAL                 
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'W'                                                             
         DS    C                                                                
         DS    CL11                   'LOCAL VIDEO'                             
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
*                                                                               
         DC    C'A'                                                             
         DS    C                                                                
         DS    CL11                   'NOT MEDIA'                               
         DS    CL4'    '                                                        
         DS    CL16'** UNASSIGNED **'                                           
         DS    C'Y'                                                             
*                                                                               
         DC    AL1(EOT)                                                         
EOT      EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* BROADCAST DATE BLOCK                                                          
***********************************************************************         
DATEBLK  DS    0C                                                               
CURBST   DS    XL2                 COMPRESSED CURRENT BROADCAST START           
CURBND   DS    XL2                 COMPRESSED CURRENT BROADCAST END             
PRIBST   DS    XL2                 COMPRESSED PRIOR BROADCAST START             
PRIBND   DS    XL2                 COMPRESSED PRIOR BROADCAST END               
***********************************************************************         
* FAKE SPACEND TABLE FOR SETVAL IN RXROUTS                                      
***********************************************************************         
STAXREC DC     XL72'0000'                                                       
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED                
***********************************************************************         
COPYBUFF DS    CL10000                                                          
         EJECT                                                                  
***********************************************************************         
* TF SSB                                                              *         
***********************************************************************         
         DS    0L                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      CSECT                                                                  
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
         EJECT                                                                  
***********************************************************************         
* COPY OF COMFACS FOR THOSE DAMN DEMOS                                *         
***********************************************************************         
COMFACS  CSECT ,                   COMMON FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(0)                CALLOFF)                                     
         DC    A(0)                GETMSG)                                      
         DC    A(0)                GETTXT)                                      
         DC    A(0)                SWITCH)                                      
         DC    A(0)                HELLO)                                       
         DC    A(0)                SCANNER)                                     
         DC    A(0)                UNSCAN)                                      
         DC    A(0)                HEXIN)                                       
         DC    V(HEXOUT)                                                        
         DC    A(0)                CASHVAL)                                     
         DC    A(0)                DATVAL)                                      
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL)                                     
         DC    A(0)                SCUNKEY)                                     
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    A(0)                GETPROF)                                     
         DC    V(PERVERT)                                                       
         DC    A(0)                GETFACT)                                     
         DC    A(0)                XSORT)                                       
         DC    A(0)                REQTWA)                                      
         DC    A(0)                GETFLD)                                      
*&&UK                                                                           
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    V(GENERAL)                                                       
         DC    18A(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    A(0)                DDISPSRT)                                    
         DC    V(DEMADDR)                                                       
         DC    A(0)                DEMDISP)                                     
         DC    A(0)                DBOOK)                                       
         DC    A(0)                DSTATION)                                    
         DC    A(0)                DMASTER)                                     
         DC    A(0)                DFORMULA)                                    
         DC    A(0)                DNAME)                                       
         DC    A(0)                DCODE)                                       
         DC    A(0)                DCONTROL)                                    
         DC    A(0)                DADJUST)                                     
         DC    A(0)                DEMOUT)                                      
         DC    A(0)                DEMEL)                                       
         DC    A(0)                DEMAINT)                                     
         DC    A(0)                DEMAND)                                      
         DC    A(0)                DEMOMATH)                                    
         DC    A(0)                DEMOVAL)                                     
         DC    A(0)                GENERAL)                                     
         DC    V(PERVAL)                                                        
         DC    A(0)                DLFLD)                                       
         DC    A(0)                                                             
*&&                                                                             
         DC    A(0)                GLOBBER)                                     
         DC    A(0)                MINIO)                                       
         DC    A(0)                PARSNIP)                                     
         DC    A(0)                DICTATE)                                     
         DC    A(0)                EDITOR)                                      
         DC    A(0)                GETHELP)                                     
         DC    A(0)                CUREDIT)                                     
         DC    A(0)                GETRET)                                      
         DC    A(0)                REPORT)                                      
         DC    A(0)                BLDCUR)                                      
         DC    A(0)                GETCUR)                                      
         DC    A(0)                GETNARR)                                     
         DC    A(0)                DEJAVU)                                      
         DC    A(0)                SECRET)                                      
         DC    A(0)                BILLIT)                                      
         DC    A(0)                                                             
         DC    A(0)                PQPROF)                                      
         DC    2A(0)                                                            
         DC    A(0)                BINSRCH)                                     
         DC    A(0)                PROTON)                                      
         DC    A(0)                PROTOFF)                                     
         DC    A(0)                HELEN)                                       
         DC    A(0)                MQIO)                                        
         DC    A(0)                EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    8A(0)               SPARE                                        
*                                                                               
*                                                                               
*                                                                               
QXTRBLK  CSECT                                                                  
*                                                                               
GETCPROF NTR1  BASE=*,LABEL=*                                                   
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(25),0(R2) SAVE CURRENT RECORD'S KEY                      
*                                                                               
* BUILD CLIENT KEY                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAMCLT),SVAMCLT                                           
         MVI   KEY+3,X'02'         PCLKTRCD, CLIENT RECORD                      
         MVC   KEYSAVE2,KEY                                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                              
         CLC   KEY(25),KEYSAVE2                                                 
         BE    *+6                                                              
         DC    H'0'                CLIENT NOT FOUND                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,GETREC,PRTFIL,KEY+27,UCOMCLT,DMWORK                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'02'        PCLTELEM                                     
         LA    R4,UCOMCLT          POINT R4 TO BEGINNING OF CLT REC             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PCLTELEM,R4                                                      
         MVC   SVCPROF,PCLTPROF                                                 
         DROP  R4                                                               
*                                                                               
* RESTORE READ SEQUENCE                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(25),KEYSAVE                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                              
         CLC   KEY(25),KEYSAVE                                                  
         JE    YES                                                              
         DC    H'0'                READ SEQUENCE RESTORE FAILED                 
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
DMWORK   DS    12D                                                              
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
VINVBUFF DS    V                   INVOICE BUFFER FOR BUY                       
PRIADDR  DS    CL4                                                              
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
KEY      DS    CL33                                                             
KEYSAVE  DS    CL33                                                             
KEYSAVE2 DS    CL33                                                             
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
ROUTFLG  DS    XL1                 'X'= NO CUSTOM COL OUTPUT FROM BUY           
*                                    USED DIFFERENTLY IN UPDTPDA                
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
PRIALPHA DS    XL2                                                              
VERSION  DS    XL1                                                              
PLATFORM DS    XL1                                                              
*                                                                               
PRIPFLG  DS    XL1                 PUB '81' OR '85'  REP '11'                   
*                                                                               
FLAGMED  DS    XL1                                                              
COPYFLAG DS    XL1                 CAN BE ONLY 00 OR 01(CHANGE)                 
PUTRFLAG DS    XL1                 PUT RECORD FLAG                              
PUTCHINV DS    XL1                 PUT CHANGE INVOICE                           
ONLYB    DS    XL1                 WILL BE ALWAYS '00'                          
*                                  ONLY FOR BUY IT WILL BE '08'                 
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
*                                                                               
SYSCODE  DS    C                                                                
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
WORK     DS    XL64                                                             
SVAMCLT  DS    CL7                                                              
SVCPROF  DS    CL32                                                             
SVCLTPRD DS    CL10                SAVED A/M/REC/CLT/PRD                        
INVBUFF  DS    XL(MAXINVQ*INVBRLQ) INVOICE BUFFER                               
INVBRLQ  EQU   L'PBNVSER#+L'PBNVDSQN                                            
MAXINVQ  EQU   60                  MAX OF 60 INVOICES PER BUY                   
INVBUFLQ EQU   *-INVBUFF                                                        
INVBUFFX DS    X                   END OF INVOICE BUFFER MARKER (X'FF')         
*                                                                               
INVNOTBL DS    XL(MAXINVQ*INVNLNQ) INVOICE NUMBER BUFFER W/ STATUS              
MAXINVNO EQU   60                  MAX OF 60 INVOICES PER BUY                   
INVNOBLQ EQU   *-INVNOTBL                                                       
INVNOTBX DS    X                   END OF BUFFER MARKER (X'FF')                 
SAVERF   DS    F                                                                
       ++INCLUDE PTXPARM           PTXTRBLK-PTXROUTS PARM BLOCK                 
*                                                                               
UCOMCLT  DS    500X                                                             
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    4096X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
PRCIVWSD DSECT                     PROCESS INVOICE WORKING STORAGE              
PIVFULL1 DS    F                                                                
PIVFULL2 DS    F                                                                
PIVDUB1  DS    D                                                                
PIVDUB2  DS    D                                                                
PIVDWORK DS    12D                                                              
PIVKEY   DS    XL(L'IOKEY)                                                      
PIVSVKEY DS    XL(L'IOKEYSAV)                                                   
PIV_INV# DS    CL(L'PNVHINV#)      INVOICE NUMBER                               
PIV_IVDT DS    XL(L'PNVHDATE)      INVOICE DATE                                 
PIV_IEDT DS    XL(L'PNVHEND)       INVOICE END DATE FOR MOS                     
PIV_DNET DS    PL(L'PNVDNET)       INVOICE ITEM NET AMOUNT                      
PRCIVWX  EQU   *                                                                
PRCIVWLQ EQU   PRCIVWX-PRCIVWSD                                                 
*                                                                               
***********************************************************************         
* DSECT TO COVER PRINT MEDIA TABLE                                    *         
***********************************************************************         
MEDTABD  DSECT                                                                  
MEDTCODE DS    C                   MEDIA CODE                                   
MEDTPROF DS    C                   MEDIA PROFILE                                
MEDTNAME DS    CL11                MEDIA NAME                                   
MEDTREP  DS    CL4                 NULL                                         
MEDTRNME DS    CL16                REP NAME                                     
MEDTRPUB DS    C                   REP PUB (Y/N)                                
MEDTLNQ  EQU   *-MEDTABD                                                        
***********************************************************************         
* DSECT TO COVER INVOICE NUMBER TABLE                                 *         
***********************************************************************         
INVND    DSECT                                                                  
INVNSTAT DS    C                   STATUS                                       
INVNINV  DS    CL14                INVOICE                                      
INVNSERL DS    XL5                 SERIAL NUMBER                                
INVNSQNO DS    XL2                 SEQUENCE NUMBER                              
INVNLNQ  EQU   *-INVND                                                          
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
TYPSTAT  DS    XL1                 STATUS BYTE                                  
TYPSSUP  EQU   X'80'               BELONGS TO SUPPORT MACRO TYPE                
         DS    XL2                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
***********************************************************************         
* DSECT TO COVER ADDRESSD                                             *         
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE PTXRBWRK                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
* RXUSERD                                                                       
       ++INCLUDE RXUSERD                                                        
* PTXBRECD                                                                      
       ++INCLUDE PTXBRECD                                                       
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
       ++INCLUDE PRGENFILE                                                      
*                                                                               
       ++INCLUDE PUCOMREC                                                       
         PRINT OFF                                                              
         PRINT ON                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
* FASSBOFF                                                                      
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108PTXTRBLK  06/27/19'                                      
         END                                                                    
