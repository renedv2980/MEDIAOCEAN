*          DATA SET ACLDXC2C   AT LEVEL 023 AS OF 09/19/17                      
*PHASE ACLDC2CA                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
ACLDXC2C TITLE 'Copy company to new company'                                    
         PRINT NOGEN                                                            
***********************************************************************         
* PARM card, from company alpha followed by to company alpha(s)       *         
***********************************************************************         
         USING WORKC,RC            RC=A(WORK AREA)                              
         USING DPRINT,RA           RA=A(PRINT AREA)                             
         USING COMFACSD,COMFACS                                                 
ACLDXC2C CSECT                                                                  
         NMOD1 0,**AC2C**                                                       
         L     RC,AWORKC                                                        
         ST    R1,APARM                                                         
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,PACPRINT                                                      
                                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
EXIT     XIT1                                                                   
                                                                                
AWORKC   DC    A(WORKC)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         USING LDDEFND,RF                                                       
         USING ISDTF,R2                                                         
DMXINIT  L     RF,PALDEFN                                                       
*                                                                               
         L     R2,LDDDTFIS                                                      
         ZAP   PKZERO,=P'0'                                                     
         ZAP   COUNT1,PKZERO                                                    
         ZAP   COUNT2,PKZERO                                                    
         MVC   CDATAMGR,LDATAMGR                                                
         MVC   CDATCON,LDATCON                                                  
         MVC   CHEXOUT,LHEXOUT                                                  
         MVC   CHEXIN,=V(HEXIN)                                                 
         MVC   CSCANNER,LSCANNER                                                
         L     RE,LUPSIVAL         A(UPSI)                                      
         MVC   UPSI,0(RE)                                                       
         MVI   LOAD,NO                                                          
*                                                                               
         L     R4,PAPARMC                                                       
         CLC   0(6,R4),=C'PARMIN'  Are the parameters in a file?                
         BNE   DMI020              No: continue                                 
         MVI   PARMFILE,C'Y'                                                    
         OPEN  (PARMIN,INPUT)      Open parameter input file                    
         LTR   RF,RF                                                            
         JNZ   *+2                 Problem opening parameter file               
*                                                                               
DMI010   L     RE,AC2CTAB          Next slot in C2C table                       
         CLI   0(RE),EOT           Any space left in the table?                 
         JE    *+2                 Need to make C2CTAB bigger                   
         GET   PARMIN,PARMCARD     Next parm from file                          
         LA    R4,PARMCARD                                                      
*                                                                               
DMI020   GOTOR CSCANNER,DMCB,(C'C',(R4)),(6,BLOCK),0                            
         CLI   4(R1),6             Always need 6 alpha,hex,alpha,hex,           
         BE    *+6                               logo,id num                    
         DC    H'00'               Need at least from and to alpha              
                                                                                
*================================                                               
* Get alpha for original company                                                
*================================                                               
         USING SCANBLKD,R6                                                      
         LA    R6,BLOCK                                                         
         CLI   SC1STLEN,2          must be length 2, from ALPHA                 
         BE    *+6                                                              
         DC    H'00'               Need at least from and to alpha              
         MVC   ALPHAOLD,SC1STFLD                                                
                                                                                
*================================                                               
* Get hex for original company                                                  
*================================                                               
         AHI   R6,SCBLKLQ                                                       
         CLI   SC1STLEN,2          must be length 2, from ALPHA                 
         BE    *+6                                                              
         DC    H'00'               Need at least from and to alpha              
         TM    SC1STVAL,SCHEXQ                                                  
         BO    *+6                                                              
         DC    H'00'               Must be hex value                            
         GOTOR CHEXIN,DMCB,SC1STFLD,CPYOLD,2,0                                  
         CLI   CPYOLD,X'40'                                                     
         BNL   *+6                                                              
         DC    H'00'               Companies are X'40' or greater               
                                                                                
*================================                                               
* Get alpha for new company                                                     
*================================                                               
         AHI   R6,SCBLKLQ                                                       
         CLI   SC1STLEN,2          must be length 2, from ALPHA                 
         BE    *+6                                                              
         DC    H'00'               Need at least from and to alpha              
         MVC   ALPHANEW,SC1STFLD                                                
                                                                                
*================================                                               
* Get hex for new company                                                       
*================================                                               
         AHI   R6,SCBLKLQ                                                       
         CLI   SC1STLEN,2          must be length 2, from ALPHA                 
         BE    *+6                                                              
         DC    H'00'               Need at least from and to alpha              
         TM    SC1STVAL,SCHEXQ                                                  
         BO    *+6                                                              
         DC    H'00'               Must be hex value                            
         GOTOR CHEXIN,DMCB,SC1STFLD,CPYNEW,2,0                                  
         CLI   CPYNEW,X'40'                                                     
         BNL   *+6                                                              
         DC    H'00'               Companies are X'40' or greater               
                                                                                
*================================                                               
* Get logo for new company                                                      
*================================                                               
         AHI   R6,SCBLKLQ                                                       
         MVC   LOGONEW,SC1STFLD                                                 
                                                                                
*================================                                               
* Get hex for new id number                                                     
*================================                                               
         AHI   R6,SCBLKLQ                                                       
         TM    SC1STVAL,SCHEXQ                                                  
         BO    *+6                                                              
         DC    H'00'               Must be hex value                            
         MVC   IDNEW,SC1STNUM+2                                                 
*                                                                               
         L     RE,AC2CTAB                                                       
         MVC   0(C2CDATAL,RE),C2CDATA  Copy to current slot in table            
*                                                                               
         CLI   PARMFILE,C'Y'                                                    
         BNE   DMINTXIT                                                         
*                                                                               
         LA    RE,C2CDATAL(,RE)        bump                                     
         ST    RE,AC2CTAB              A(Next available entry)                  
         B     DMI010                                                           
*                                                                               
PARMEND  CLOSE PARMIN              Close parameter input file                   
         MVC   AC2CTAB,=A(C2CTAB)  Reset A(C2C table)                           
*                                                                               
DMINTXIT J     EXIT                                                             
         DROP  R2,RF                                                            
                                                                                
***********************************************************************         
* PROCESS A RECORD                                                    *         
***********************************************************************         
         USING ACTRECD,R2                                                       
DMXREC   DS    0H                                                               
         SR    R6,R6                                                            
         TM    PLIST+4,X'10'                                                    
         BO    DMREC010                                                         
         L     R2,PADAREC                                                       
         TM    ACTRSTA,ACTSDELT                                                 
         BO    DMXPURGE                                                         
         B     DMREC020                                                         
                                                                                
DMREC010 L     R2,PADSISP                                                       
         CLI   0(R2),PLDKTYPQ      X'18'                                        
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
DMREC020 GOTOR VRECTYPE,DMCB,(C'D',ACTRECD)                                     
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
*                                                                               
         L     RE,AC2CTAB          First entry in the C2C table                 
DMREC030 MVC   C2CDATA(C2CDATAL),0(RE) Set the current data                     
         CLI   CPYOLD,EOR          Did we find the company we want?             
         BE    DMXPURGE            no: purge                                    
         CLI   CPYOLD,EOT          Did we hit the end of the list?              
         BE    DMXPURGE            Yes: purge                                   
         CLC   COMPANY,CPYOLD      FROM company?                                
         BE    CRCPYKEY            yes: continue with this data                 
         LA    RE,C2CDATAL(,RE)    Bump to next entry                           
         B     DMREC030                                                         
                                                                                
*        CLI   RECTYPE,ACRTCPY     Is this a company record?                    
*        BE    DMXPURGE                                                         
                                                                                
***********************************************************************         
* Create copy of key                                                            
***********************************************************************         
CRCPYKEY SR    RE,RE                                                            
         ICM   RE,1,2(R1)          Displacement to company hex value            
         AR    RE,R2               Point to company hex value                   
         MVC   0(1,RE),CPYNEW      To company                                   
*                                                                               
         LLC   R1,RECTYPE                                                       
         LA    R4,RECTYPES(R1)     Point to table entry value                   
         SR    RE,RE                                                            
         ICM   RE,1,0(R4)          Table entry available                        
         BZ    CHKELEMS            No, so check elements now                    
         CLI   0(R4),X'FF'         Get routine #                                
         BE    ROUTINE2                                                         
         AR    RE,R2                                                            
         CLC   COMPANY,0(RE)                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   0(1,RE),CPYNEW                                                   
         EJECT ,                                                                
***********************************************************************         
* Check and change elements                                                     
***********************************************************************         
CHKELEMS DS    0H                                                               
         LA    R3,ACTRFST          Point to elements                            
CHKELM10 CLI   0(R3),EOR           End of record                                
         BE    DMXKEEP                                                          
         LLC   R1,0(R3)            Get element code                             
         LA    R4,ELMTYPES(R1)     Point to table entry value                   
         SR    RE,RE                                                            
         ICM   RE,1,0(R4)          Table entry available                        
         BZ    CHKELM90                                                         
         CLI   0(R4),X'FF'         Get routine #                                
         BE    ROUTINE                                                          
         AR    RE,R3                                                            
         CLI   0(RE),0             If zero skip                                 
         BE    CHKELM90                                                         
         CLC   COMPANY,0(RE)                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   0(1,RE),CPYNEW                                                   
                                                                                
CHKELM90 LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     CHKELM10                                                         
         EJECT ,                                                                
***********************************************************************         
* Special element routines                                                      
***********************************************************************         
ROUTINE  DS    0H                                                               
         LA    R5,ROUTTAB                                                       
         LLC   R1,0(R3)            Get element code                             
         LA    R4,ROUTTAB(R1)      Point to table entry value                   
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)                                                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     RCPYEL              Company                                      
         B     RPMDEL              Prod media                                   
         B     RPPREL              Prod profile                                 
         B     RPBAEL              Pool brand allocation                        
         B     RCACEL              Contra account header                        
         B     ROCNEL              Office check number                          
         B     RGLDEL              General ledger detail                        
         B     RORDEL              Production order                             
         B     RBICEL              Batch item check                             
         B     RPTREL              Pointer                                      
         B     RASKEL                                                           
         B     ROUTINEX                                                         
                                                                                
ROUTINEX B     CHKELM90            Next element                                 
         EJECT ,                                                                
         USING CPYELD,R3                                                        
RCPYEL   DS    0H                                                               
         MVC   CPYLOGO,LOGONEW                                                  
         MVC   CPYALPHA,ALPHANEW                                                
         MVC   CPYUID,IDNEW                                                     
         B     ROUTINEX                                                         
                                                                                
         USING PMDELD,R3                                                        
RPMDEL   DS    0H                                                               
         CLC   COMPANY,PMDCOMC1                                                 
         BNE   *+10                                                             
         MVC   PMDCOMC1,CPYNEW                                                  
         CLC   COMPANY,PMDVATC1                                                 
         BNE   *+10                                                             
         MVC   PMDVATC1,CPYNEW                                                  
         CLC   COMPANY,PMDCSHDC                                                 
         BNE   *+10                                                             
         MVC   PMDCSHDC,CPYNEW                                                  
*&&UK                                                                           
         CLC   COMPANY,PMDVATC2                                                 
         BNE   *+10                                                             
         MVC   PMDVATC2,CPYNEW                                                  
         CLC   COMPANY,PMDCOMC2                                                 
         BNE   *+10                                                             
         MVC   PMDCOMC2,CPYNEW                                                  
         CLC   COMPANY,PMDFLTC2                                                 
         BNE   *+10                                                             
         MVC   PMDFLTC2,CPYNEW                                                  
*&&                                                                             
         B     ROUTINEX                                                         
*                                                                               
         USING PPRELD,R3                                                        
RPPREL   DS    0H                                                               
         CLC   COMPANY,PPRRECVC                                                 
         BNE   *+10                                                             
         MVC   PPRRECVC,CPYNEW                                                  
         CLC   COMPANY,PPRCOSTC                                                 
         BNE   *+10                                                             
         MVC   PPRCOSTC,CPYNEW                                                  
         B     ROUTINEX                                                         
*                                                                               
         USING PBAELD,R3                                                        
RPBAEL   DS    0H                                                               
         CLC   COMPANY,PBARECVC                                                 
         BNE   *+10                                                             
         MVC   PBARECVC,CPYNEW                                                  
         CLC   COMPANY,PBACOSTC                                                 
         BNE   *+10                                                             
         MVC   PBACOSTC,CPYNEW                                                  
         B     ROUTINEX                                                         
*                                                                               
         USING CACELD,R3                                                        
RCACEL   DS    0H                                                               
         CLC   COMPANY,CACCNTC                                                  
         BNE   *+10                                                             
         MVC   CACCNTC,CPYNEW                                                   
         B     ROUTINEX                                                         
*                                                                               
         USING OCNELD,R3                                                        
ROCNEL   DS    0H                                                               
         CLI   OCNLN,OCNLN3Q                                                    
         BNE   *+10                                                             
         MVC   OCNBANKC,CPYNEW                                                  
         B     ROUTINEX                                                         
*                                                                               
         USING GLDELD,R3                                                        
RGLDEL   DS    0H                                                               
         CLI   GLDLN,GLDLNQ                                                     
         BNE   *+10                                                             
         MVC   GLDCCPY,CPYNEW                                                   
         B     ROUTINEX                                                         
*                                                                               
         USING ORDELD,R3                                                        
RORDEL   DS    0H                                                               
         CLC   COMPANY,ORDACCC                                                  
         BNE   *+10                                                             
         MVC   ORDACCC,CPYNEW                                                   
         CLC   COMPANY,ORDSUPC                                                  
         BNE   *+10                                                             
         MVC   ORDSUPC,CPYNEW                                                   
         B     ROUTINEX                                                         
*                                                                               
         USING BICELD,R3                                                        
RBICEL   DS    0H                                                               
         CLC   COMPANY,BICACTC                                                  
         BNE   *+10                                                             
         MVC   BICACTC,CPYNEW                                                   
         CLC   COMPANY,BICCACC                                                  
         BNE   *+10                                                             
         MVC   BICCACC,CPYNEW                                                   
         B     ROUTINEX                                                         
*                                                                               
         USING PTRELD,R3                                                        
         USING RAPRECD,R7                                                       
RPTREL   DS    0H                                                               
         CLI   PTRTYPE,PTRTRAP     Is this a pointer?                           
         BNE   PTRELX                                                           
         LA    R7,PTRCODE                                                       
         CLC   RAPKCPY,COMPANY                                                  
         BNE   PTRELX                                                           
         MVC   RAPKCPY,CPYNEW      Move in company code                         
*                                                                               
* Move in company code to correct position in key (if applicable)               
*                                                                               
         CLI   RAPKRTYP,RAPKROFF   X'01'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKROGR   X'02'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKRCLI   X'03'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRPRO   X'04'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRJOB   X'05'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRSUP   X'06'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRMED   X'07'                                        
         BE    PTR20                                                            
         CLI   RAPKRTYP,RAPKRMGR   X'08'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKRWRK   X'09'                                        
         BE    PTR20                                                            
         CLI   RAPKRTYP,RAPKRWGR   X'0A'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKROPT   X'0C'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKRSCH   X'0D'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKRCAT   X'0E'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKRORD   X'0F'                                        
         BE    PTR20                                                            
         CLI   RAPKRTYP,RAPKROFL   X'10'                                        
         BE    PTRELX                                                           
         CLI   RAPKRTYP,RAPKRTAX   X'11'                                        
         BE    PTR20                                                            
         CLI   RAPKRTYP,RAPKRUSR   X'12'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKR1RA   X'14'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKR1RB   X'15'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKR1RC   X'16'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKR1RD   X'17'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRPER   X'18'                                        
         BE    PTR20                                                            
         CLI   RAPKRTYP,RAPKRLED   X'19'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRRAT   X'1A'                                        
         BE    PTR20                                                            
         CLI   RAPKRTYP,RAPKRPAL   X'1B'                                        
         BE    PTR10                                                            
         CLI   RAPKRTYP,RAPKRAPP   X'1C'                                        
         BE    PTR30                                                            
         CLI   RAPKRTYP,RAPKRART   X'1D'                                        
         BE    PTR30                                                            
         DC    H'0'                Die if not one of these types                
PTR10    CLC   RAPKACPY,COMPANY                                                 
         BNE   PTRELX                                                           
         MVC   RAPKACPY,CPYNEW                                                  
         B     PTRELX                                                           
PTR20    CLC   COMPANY,RAPKKEY+1                                                
         BNE   PTRELX                                                           
         MVC   RAPKKEY+1(1),CPYNEW                                              
         B     PTRELX                                                           
PTR30    CLC   COMPANY,RAPKKEY+2                                                
         BNE   PTRELX                                                           
         MVC   RAPKKEY+2(1),CPYNEW                                              
*                                                                               
PTRELX   B     ROUTINEX                                                         
                                                                                
         USING ASKELD,R3                                                        
         USING TRNRECD,RE                                                       
RASKEL   DS    0H                                                               
         LA    RE,ASKKEY                                                        
         CLC   COMPANY,TRNKCPY                                                  
         BNE   *+10                                                             
         MVC   TRNKCPY,CPYNEW                                                   
         CLC   COMPANY,TRNKCCPY                                                 
         BNE   *+10                                                             
         MVC   TRNKCCPY,CPYNEW                                                  
         B     ROUTINEX                                                         
         DROP  RE                                                               
         EJECT ,                                                                
***********************************************************************         
* Special key routines                                                          
***********************************************************************         
ROUTINE2 DS    0H                                                               
         LA    R5,ROU2TAB                                                       
         LLC   R1,RECTYPE                                                       
         LA    R4,ROU2TAB(R1)      Point to table entry value                   
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)                                                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     RCHSREC             Contra header                                
         B     RCACREC             Contra account                               
         B     RTRNREC             Transaction                                  
         B     RBUDREC             Budget                                       
                                                                                
ROUTIN2X B     CHKELEMS            Go to elements                               
         EJECT ,                                                                
         USING BUDRECD,R2                                                       
RBUDREC  DS    0H                                                               
         CLC   COMPANY,CPYNEW                                                   
         BE    ROUTIN2X                                                         
         CLI   BUDKCCPY,X'40'                                                   
         BNH   ROUTIN2X                                                         
         CLC   BUDKCCPY,COMPANY                                                 
         BE    RBUDR10                                                          
         AP    BERROR,=P'1'                                                     
         CP    BERROR,=P'21'                                                    
         BH    ROUTIN2X                                                         
         GOTOR VPRNTBL,DMCB,0,(R2),C'DUMP',56,=C'1R',(C'P',VPRINT)              
         B     ROUTIN2X                                                         
*                                                                               
RBUDR10  MVC   BUDKCCPY,CPYNEW                                                  
         B     ROUTIN2X                                                         
*                                                                               
         USING CACRECD,R2                                                       
RCACREC  DS    0H                                                               
         CLC   COMPANY,CPYNEW                                                   
         BE    ROUTIN2X                                                         
         CLI   CACKCCPY,X'40'                                                   
         BNH   ROUTIN2X                                                         
         CLC   CACKCCPY,COMPANY                                                 
         BE    RCACR10                                                          
         CLC   CACKCCPY,CPYNEW                                                  
         BE    ROUTIN2X                                                         
         CLI   CACKCCPY,C'*'                                                    
         BE    ROUTIN2X                                                         
         AP    CERROR,=P'1'                                                     
         CP    CERROR,=P'21'                                                    
         BH    ROUTIN2X                                                         
         GOTOR VPRNTBL,DMCB,0,(R2),C'DUMP',56,=C'1R',(C'P',VPRINT)              
         B     ROUTIN2X                                                         
*                                                                               
RCACR10  MVC   CACKCCPY,CPYNEW                                                  
         B     ROUTIN2X                                                         
*                                                                               
         USING TRNRECD,R2                                                       
RTRNREC  DS    0H                                                               
         CLC   COMPANY,CPYNEW                                                   
         BE    ROUTIN2X                                                         
         CLI   TRNKCCPY,X'40'                                                   
         BNH   ROUTIN2X                                                         
         CLC   TRNKCCPY,COMPANY                                                 
         BE    RTRNR10                                                          
         CLC   TRNKCCPY,CPYNEW                                                  
         BE    ROUTIN2X                                                         
         CLI   TRNKCCPY,C'*'                                                    
         BE    ROUTIN2X                                                         
         AP    TERROR,=P'1'                                                     
         CP    TERROR,=P'21'                                                    
         BH    ROUTIN2X                                                         
         GOTOR VPRNTBL,DMCB,0,(R2),C'DUMP',56,=C'1R',(C'P',VPRINT)              
         B     ROUTIN2X                                                         
*                                                                               
RTRNR10  MVC   TRNKCCPY,CPYNEW                                                  
         B     ROUTIN2X                                                         
*                                                                               
         USING CHDRECD,R2                                                       
RCHSREC  DS    0H                                                               
         CLC   COMPANY,CPYNEW                                                   
         BE    ROUTIN2X                                                         
         CLI   CHDKCCPY,X'40'                                                   
         BNH   ROUTIN2X                                                         
         CLC   CHDKCCPY,COMPANY                                                 
         BE    RCHSR10                                                          
         CLC   CHDKCCPY,CPYNEW                                                  
         BE    ROUTIN2X                                                         
         CLI   CHDKCCPY,C'*'                                                    
         BE    ROUTIN2X                                                         
         AP    HERROR,=P'1'                                                     
         CP    HERROR,=P'21'                                                    
         BH    ROUTIN2X                                                         
         GOTOR VPRNTBL,DMCB,0,(R2),C'DUMP',56,=C'1R',(C'P',VPRINT)              
         B     ROUTIN2X                                                         
*                                                                               
RCHSR10  MVC   CHDKCCPY,CPYNEW                                                  
         B     ROUTIN2X                                                         
         EJECT ,                                                                
***********************************************************************         
* Record type table of company code in 2nd place in key                         
***********************************************************************         
RECTYPES DC    XL256'00'                                                        
         ORG   RECTYPES+ACRTCHDH                                                
         DC    X'FF'               Set to routine                               
         ORG   RECTYPES+ACRTCAC                                                 
         DC    X'FF'               Set to routine                               
         ORG   RECTYPES+ACRTTRN                                                 
         DC    X'FF'               Set to routine                               
         ORG   RECTYPES+ACRTTRNA                                                
         DC    AL1(TRNKCCPY-TRNRECD)                                            
         ORG   RECTYPES+ACRTBUD                                                 
         DC    X'FF'               Set to routine                               
         ORG   RECTYPES+ACRTTDT                                                 
         DC    AL1(TSIKCCPY-TSIRECD)                                            
         ORG   RECTYPES+ACRTTIM                                                 
         DC    AL1(TIMKCCPY-TIMRECD)                                            
         ORG   RECTYPES+256        Reset address to end of table                
         EJECT ,                                                                
***********************************************************************         
* element table, location of company to change in element                       
***********************************************************************         
ELMTYPES DC    XL256'00'                                                        
         ORG   ELMTYPES+CPYELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+PMDELQ                                                  
         DC    X'FF'               Set to routine                               
*&&UK                                                                           
         ORG   ELMTYPES+LDGELQ                                                  
         DC    AL1(LDGCDSCC-LDGELD)                                             
*&&                                                                             
         ORG   ELMTYPES+PPRELQ                                                  
         DC    X'FF'               Set to routine                               
*        ORG   ELMTYPES+CEXELQ                                                  
*        DC    AL1(CEXACCC-CEXELD)                                              
         ORG   ELMTYPES+PBAELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+CACELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+PXDELQ                                                  
         DC    AL1(PXDFRTOC-PXDELD)                                             
         ORG   ELMTYPES+OCNELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+APTELQ                                                  
         DC    AL1(APTACCC-APTELD)                                              
         ORG   ELMTYPES+ASKELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+GLDELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+ORDELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+BICELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+PTRELQ                                                  
         DC    X'FF'               Set to routine                               
         ORG   ELMTYPES+256        Reset address to end of table                
         EJECT ,                                                                
***********************************************************************         
* Routine to deal with specific element                                         
***********************************************************************         
ROUTTAB  DC    XL256'00'                                                        
         ORG   ROUTTAB+CPYELQ                                                   
         DC    AL1(1)              Set to routine                               
         ORG   ROUTTAB+PMDELQ                                                   
         DC    AL1(2)              Set to routine                               
         ORG   ROUTTAB+PPRELQ                                                   
         DC    AL1(3)              Set to routine                               
         ORG   ROUTTAB+PBAELQ                                                   
         DC    AL1(4)              Set to routine                               
         ORG   ROUTTAB+CACELQ                                                   
         DC    AL1(5)              Set to routine                               
         ORG   ROUTTAB+OCNELQ                                                   
         DC    AL1(6)              Set to routine                               
         ORG   ROUTTAB+GLDELQ                                                   
         DC    AL1(7)              Set to routine                               
         ORG   ROUTTAB+ORDELQ                                                   
         DC    AL1(8)              Set to routine                               
         ORG   ROUTTAB+BICELQ                                                   
         DC    AL1(9)              Set to routine                               
         ORG   ROUTTAB+PTRELQ                                                   
         DC    AL1(10)             Set to routine                               
         ORG   ROUTTAB+ASKELQ                                                   
         DC    AL1(11)             Set to routine                               
         ORG   ROUTTAB+256         Reset address to end of table                
         EJECT ,                                                                
***********************************************************************         
* Routine to deal with specific rectypes                                        
***********************************************************************         
ROU2TAB  DC    XL256'00'                                                        
         ORG   ROU2TAB+ACRTCHDH                                                 
         DC    AL1(1)              Set to routine                               
         ORG   ROU2TAB+ACRTCAC                                                  
         DC    AL1(2)              Set to routine                               
         ORG   ROU2TAB+ACRTTRN                                                  
         DC    AL1(3)              Set to routine                               
         ORG   ROU2TAB+ACRTBUD                                                  
         DC    AL1(4)              Set to routine                               
         ORG   ROU2TAB+256         Reset address to end of table                
         EJECT ,                                                                
***********************************************************************         
* Load                                                                          
***********************************************************************         
LOAD100  CLI   LOAD,YES                                                         
         BE    *+6                                                              
         DC    H'00'               Missing UPSI switches                        
                                                                                
         CLI   RECTYPE,ACRTTRLA    Trailer ?                                    
         BNE   *+8                 No                                           
         MVI   RESUME,YES                                                       
         CLI   RESUME,YES          Drop until resume=YES                        
         BE    DMXKEEP                                                          
LOAD110  CLC   COMPANY,0(R3)       Drop all for this company                    
         BE    DMXPURGE                                                         
         AHI   R3,1                                                             
         BCT   R0,LOAD110                                                       
         B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         MVC   P+1(7),=C'BERROR='                                               
         OI    BERROR+L'BERROR-1,X'0F'                                          
         UNPK  P+8(8),BERROR                                                    
         GOTO1 VPRINT,DMCB,P,BC01                                               
*                                                                               
         MVC   P+1(7),=C'CERROR='                                               
         OI    CERROR+L'CERROR-1,X'0F'                                          
         UNPK  P+8(8),CERROR                                                    
         GOTO1 VPRINT,DMCB,P,BC01                                               
*                                                                               
         MVC   P+1(7),=C'TERROR='                                               
         OI    TERROR+L'TERROR-1,X'0F'                                          
         UNPK  P+8(8),TERROR                                                    
         GOTO1 VPRINT,DMCB,P,BC01                                               
*                                                                               
         MVC   P+1(7),=C'HERROR='                                               
         OI    HERROR+L'HERROR-1,X'0F'                                          
         UNPK  P+8(8),HERROR                                                    
         GOTO1 VPRINT,DMCB,P,BC01                                               
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         J     EXIT                                                             
                                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         J     EXIT                                                             
                                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         J     EXIT                                                             
                                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         J     EXIT                                                             
                                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         J     EXIT                                                             
         EJECT ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
PARMIN   DCB   DDNAME=PARMIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      *        
               EODAD=PARMEND                                                    
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKC    CSECT                                                                  
                                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
EOR      EQU   0                                                                
K        EQU   1024                                                             
MAXDUMP  DC    P'50'                                                            
BERROR   DC    PL8'0'                                                           
CERROR   DC    PL8'0'                                                           
TERROR   DC    PL8'0'                                                           
HERROR   DC    PL8'0'                                                           
                                                                                
VRECTYPE DC    V(ACRECTYP)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
VPRINT   DC    V(PRINT)                                                         
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMGETR   DC    C'GETREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
                                                                                
BC01     DC    C'BC01'                                                          
DUB      DS    D                                                                
DUB2     DS    D                                                                
WORK     DS    XL256                                                            
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PKZERO   DS    PL1                                                              
APARM    DS    A                                                                
AC2CTAB  DC    A(C2CTAB)                                                        
                                                                                
COMFACS  DS    24A                                                              
                                                                                
PLIST    DS    0X                                                               
PADAREC  DS    A                                                                
PATAPEO  DS    A                                                                
PAPARMC  DS    A                                                                
PALDEFN  DS    A                                                                
PAPRINT  DS    A                                                                
PACPRINT DS    A                                                                
         DS    2A                                                               
PADSISP  DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
                                                                                
SVRE     DS    A                                                                
SVRF     DS    A                                                                
                                                                                
COUNT1   DS    PL3                                                              
COUNT2   DS    PL3                                                              
*                                                                               
PARMCARD DC    CL80' '                                                          
PARMFILE DC    C'N'                Default to no file for parameters            
C2CTAB   DC    20XL(C2CDATAL)'00'  Company to company table                     
C2CTABX  DC    AL1(EOT)            Mark the end of table                        
C2CDATA  DS    0X                  Current entry for company data               
ALPHAOLD DC    CL2' '                                                           
CPYOLD   DC    X'00'                                                            
ALPHANEW DC    CL2' '                                                           
CPYNEW   DC    X'00'                                                            
LOGONEW  DC    CL7' '                                                           
IDNEW    DC    XL2'00'                                                          
C2CDATAL EQU   *-C2CDATA                                                        
                                                                                
RECTYPE  DS    X                   EXTRACTED RECORD TYPE                        
COMPANY  DS    X                                                                
DROP     DS    X                   Company to drop in load 1st round            
KEEP     DS    X                   Company to keep in dump                      
                                                                                
DUMP     DS    C                   Yes/No dumping file                          
LOAD     DS    C                   Yes/No loading file                          
RESUME   DS    C                   Yes/No - resume keeping all records          
ACCRECS  DS    C                   Yes/No - Account records processed           
                                                                                
UPSI     DS    XL1                                                              
UPSIDUMP EQU   X'80'               .   Dump list of companies                   
UPSILOAD EQU   X'40'               .   Load list of companies                   
                                                                                
BLOCK    DS    6CL(SCBLKLQ)                                                     
                                                                                
***********************************************************************         
* ++INCLUDES                                                                    
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDBUFFD                                                                       
*        PRINT OFF                                                              
*        INCLUDE DDBUFFD                                                        
*        PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACLDXC2C  09/19/17'                                      
         END                                                                    
