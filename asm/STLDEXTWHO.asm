*          DATA SET STLDEXTWHO AT LEVEL 007 AS OF 08/18/00                      
*PHASE STEXTWHO                                                                 
         TITLE 'STLDEXT - DELETES SOME RECORDS FOR SPECIFIC STATIONS'           
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
*          DATA SET SPLDEXTCBL AT LEVEL 139 AS OF 03/18/98                      
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLI   0(R3),C'A'          ADDRESS RECORD?                              
         BE    DMXR10                                                           
         CLI   0(R3),C'S'          MASTER RECORD?                               
         BNE   DMXKEEP                                                          
*                                                                               
DMXR10   CLC   =C'QU',7(R3)        FOR SQUARE ONE?                              
         BNE   DMXKEEP                                                          
         LA    R1,STATBLRA         FOR RADIO                                    
         CLI   1(R3),C'R'                                                       
         BE    DMXR20                                                           
         LA    R1,STATBLTV         FOR TELEVISION                               
         CLI   1(R3),C'T'                                                       
         BNE   DMXKEEP                                                          
DMXR20   CLI   0(R1),0                                                          
         BE    DMXKEEP                                                          
         CLC   0(5,R1),2(R3)       MATCH ON STATION?                            
         BE    DMXR30                                                           
         AHI   R1,5                                                             
         B     DMXR20                                                           
*                                                                               
DMXR30   MVC   P(20),=C'STAFIL REC DELETED: '                                   
         MVC   P+20(9),0(R3)                                                    
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
RECSCHGD DC    PL8'0'                                                           
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
TABCOUNT DS    F            MARKT/STAT                                          
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMED  DS    CL1                                                              
*                                                                               
STATBLRA DS    0CL5                                                             
         DC    C'ABC A'                                                         
         DC    C'ALA F'                                                         
         DC    C'CHARA'                                                         
         DC    C'CIDRF'                                                         
         DC    C'CIMXF'                                                         
         DC    C'CNETF'                                                         
         DC    C'EZSMF'                                                         
         DC    C'INTEF'                                                         
         DC    C'IRN A'                                                         
         DC    C'LEARF'                                                         
         DC    C'MALBF'                                                         
         DC    C'MAMAF'                                                         
         DC    C'MATLF'                                                         
         DC    C'MAUSF'                                                         
         DC    C'MBIRF'                                                         
         DC    C'MBNYF'                                                         
         DC    C'MCHAF'                                                         
         DC    C'MCHIF'                                                         
         DC    C'MCINF'                                                         
         DC    C'MCLEF'                                                         
         DC    C'MCOLF'                                                         
         DC    C'MCSPF'                                                         
         DC    C'MDA F'                                                         
         DC    C'MDV F'                                                         
         DC    C'MED A'                                                         
         DC    C'MELPF'                                                         
         DC    C'MGRAF'                                                         
         DC    C'MGSCF'                                                         
         DC    C'MHOUF'                                                         
         DC    C'MINDF'                                                         
         DC    C'MKANF'                                                         
         DC    C'MLA F'                                                         
         DC    C'MLUBF'                                                         
         DC    C'MMCSF'                                                         
         DC    C'MMILF'                                                         
         DC    C'MNASF'                                                         
         DC    C'MNWOF'                                                         
         DC    C'MORLF'                                                         
         DC    C'MPH F'                                                         
         DC    C'MPITF'                                                         
         DC    C'MPORF'                                                         
         DC    C'MRICF'                                                         
         DC    C'MRNCF'                                                         
         DC    C'MSACF'                                                         
         DC    C'MSEAF'                                                         
         DC    C'MSF F'                                                         
         DC    C'MSL F'                                                         
         DC    C'MSLCF'                                                         
         DC    C'MSTXF'                                                         
         DC    C'MTAMF'                                                         
         DC    C'MTCLA'                                                         
         DC    C'MTCSA'                                                         
         DC    C'MTDCF'                                                         
         DC    C'MTN F'                                                         
         DC    C'MTROF'                                                         
         DC    C'MULTA'                                                         
         DC    C'MWFTF'                                                         
         DC    C'NPR F'                                                         
         DC    C'PREMA'                                                         
         DC    C'REDSF'                                                         
         DC    C'SDA F'                                                         
         DC    C'SDETF'                                                         
         DC    C'SDV F'                                                         
         DC    C'SHA F'                                                         
         DC    C'SHADA'                                                         
         DC    C'SHADF'                                                         
         DC    C'SHDWF'                                                         
         DC    C'SHNYF'                                                         
         DC    C'SHOUF'                                                         
         DC    C'SLA F'                                                         
         DC    C'SMFLF'                                                         
         DC    C'SMINF'                                                         
         DC    C'SPH F'                                                         
         DC    C'SSACF'                                                         
         DC    C'STBAF'                                                         
         DC    C'STLAF'                                                         
         DC    C'STSDF'                                                         
         DC    C'STSFF'                                                         
         DC    C'SWDCF'                                                         
         DC    C'TRANA'                                                         
         DC    C'TRATF'                                                         
         DC    C'TRCLF'                                                         
         DC    C'TRDAF'                                                         
         DC    C'TRDCF'                                                         
         DC    C'TRFRF'                                                         
         DC    C'TRFXF'                                                         
         DC    C'TRINF'                                                         
         DC    C'TRKCF'                                                         
         DC    C'TRLAF'                                                         
         DC    C'TRMIF'                                                         
         DC    C'TRNHF'                                                         
         DC    C'TRNPF'                                                         
         DC    C'TRORF'                                                         
         DC    C'TRPXF'                                                         
         DC    C'TRRAF'                                                         
         DC    C'TRSAF'                                                         
         DC    C'TRSEF'                                                         
         DC    C'TRSFF'                                                         
         DC    C'TRSTF'                                                         
         DC    C'TRSTF'                                                         
         DC    C'TRTAF'                                                         
         DC    C'TRTUF'                                                         
         DC    C'TRWPF'                                                         
         DC    C'VCFRA'                                                         
         DC    C'VDPMA'                                                         
         DC    C'VINTF'                                                         
         DC    C'VKATF'                                                         
         DC    C'VMSGA'                                                         
         DC    C'VMT A'                                                         
         DC    C'VSRRA'                                                         
         DC    C'VST A'                                                         
         DC    C'VTSPA'                                                         
         DC    C'XCFBF'                                                         
         DC    C'XCMXF'                                                         
         DC    C'XEFVA'                                                         
         DC    C'XEJ A'                                                         
         DC    C'XEMBA'                                                         
         DC    C'XEMWA'                                                         
         DC    C'XEPZA'                                                         
         DC    C'XEPZF'                                                         
         DC    C'XEWRA'                                                         
         DC    C'XEWRF'                                                         
         DC    C'XEWVA'                                                         
         DC    C'XEWVF'                                                         
         DC    C'XEYXA'                                                         
         DC    C'XHCRF'                                                         
         DC    C'XHEMF'                                                         
         DC    C'XHFJF'                                                         
         DC    C'XHGUF'                                                         
         DC    C'XHH F'                                                         
         DC    C'XHIMF'                                                         
         DC    C'XHNZF'                                                         
         DC    C'XHPXF'                                                         
         DC    C'XHRMF'                                                         
         DC    C'XHSLF'                                                         
         DC    C'XHTZF'                                                         
         DC    C'XLTNF'                                                         
         DC    C'XMDRF'                                                         
         DC    C'XMGFF'                                                         
         DC    C'XMO F'                                                         
         DC    C'XMORF'                                                         
         DC    C'XOCLF'                                                         
         DC    C'XOMXF'                                                         
         DC    C'XQIKF'                                                         
         DC    C'XSHEF'                                                         
         DC    C'XSOLF'                                                         
         DC    C'XTIMF'                                                         
         DC    C'XTRAA'                                                         
         DC    C'XTRAF'                                                         
         DC    C'XWKAF'                                                         
         DC    C'XXXLF'                                                         
         DC    C'ZARMF'                                                         
         DC    C'ZCFBF'                                                         
         DC    C'ZEDRF'                                                         
         DC    C'ZGLOF'                                                         
         DC    C'ZIOVF'                                                         
         DC    C'ZJHMF'                                                         
         DC    C'ZJRRF'                                                         
         DC    C'ZMGFF'                                                         
         DC    C'ZMMOF'                                                         
         DC    C'ZOCLF'                                                         
         DC    C'ZOGLF'                                                         
         DC    C'ZOMXF'                                                         
         DC    C'ZPBGF'                                                         
         DC    C'ZPOWF'                                                         
         DC    C'ZRVVF'                                                         
         DC    C'ZSHEF'                                                         
         DC    C'ZSOXF'                                                         
         DC    C'ZSWTF'                                                         
         DC    C'ZWCTF'                                                         
         DC    C'ZWKAF'                                                         
         DC    C'ZXXLF'                                                         
         DC    C'ZZTAF'                                                         
         DC    X'00'                                                            
*                                                                               
STATBLTV DS    0CL5                                                             
         DC    C'ABXTT'                                                         
         DC    C'ACHWT'                                                         
         DC    C'ADCMT'                                                         
         DC    C'ADLAT'                                                         
         DC    C'AIWBT'                                                         
         DC    C'ANBET'                                                         
         DC    C'AWBBT'                                                         
         DC    C'AWTLT'                                                         
         DC    C'AWZBT'                                                         
         DC    C'AZWBT'                                                         
         DC    C'BAKET'                                                         
         DC    C'BUCKT'                                                         
         DC    C'CAGOT'                                                         
         DC    C'CAPNT'                                                         
         DC    C'CCA T'                                                         
         DC    C'CCP T'                                                         
         DC    C'CCPHT'                                                         
         DC    C'CCSBT'                                                         
         DC    C'CLTVT'                                                         
         DC    C'CNI T'                                                         
         DC    C'CNIAT'                                                         
         DC    C'CNIBT'                                                         
         DC    C'CNIHT'                                                         
         DC    C'CNINT'                                                         
         DC    C'CNIPT'                                                         
         DC    C'CNIRT'                                                         
         DC    C'CNIYT'                                                         
         DC    C'COMCT'                                                         
         DC    C'COXNT'                                                         
         DC    C'CSN T'                                                         
         DC    C'DET T'                                                         
         DC    C'DETRT'                                                         
         DC    C'EZSMT'                                                         
         DC    C'FSNCT'                                                         
         DC    C'FSNST'                                                         
         DC    C'FSNWT'                                                         
         DC    C'FSRMT'                                                         
         DC    C'FSWLT'                                                         
         DC    C'FXCHT'                                                         
         DC    C'FXDTT'                                                         
         DC    C'FXNWT'                                                         
         DC    C'FXNYT'                                                         
         DC    C'FXPIT'                                                         
         DC    C'GBCAT'                                                         
         DC    C'GBCBT'                                                         
         DC    C'GBCNT'                                                         
         DC    C'GBCST'                                                         
         DC    C'GBNTT'                                                         
         DC    C'GDRXT'                                                         
         DC    C'GDTVT'                                                         
         DC    C'GFTVT'                                                         
         DC    C'GFWDT'                                                         
         DC    C'GMSGT'                                                         
         DC    C'GRPMT'                                                         
         DC    C'GRPWT'                                                         
         DC    C'GRWAT'                                                         
         DC    C'GSTST'                                                         
         DC    C'GSWTT'                                                         
         DC    C'GTMDT'                                                         
         DC    C'GTVWT'                                                         
         DC    C'GUVNT'                                                         
         DC    C'GUVST'                                                         
         DC    C'GVDAT'                                                         
         DC    C'GWEXT'                                                         
         DC    C'GXLNT'                                                         
         DC    C'HCTVT'                                                         
         DC    C'HISCT'                                                         
         DC    C'HTSBT'                                                         
         DC    C'MEDOT'                                                         
         DC    C'MIL T'                                                         
         DC    C'MSGNT'                                                         
         DC    C'NCA T'                                                         
         DC    C'NCAAT'                                                         
         DC    C'NCABT'                                                         
         DC    C'NCAHT'                                                         
         DC    C'NCAMT'                                                         
         DC    C'NCANT'                                                         
         DC    C'NCAPT'                                                         
         DC    C'NCAST'                                                         
         DC    C'NCAVT'                                                         
         DC    C'NCAYT'                                                         
         DC    C'NCC T'                                                         
         DC    C'NCCBT'                                                         
         DC    C'NCCLT'                                                         
         DC    C'NCHET'                                                         
         DC    C'NECNT'                                                         
         DC    C'NESNT'                                                         
         DC    C'NEWTT'                                                         
         DC    C'NNE T'                                                         
         DC    C'NWCNT'                                                         
         DC    C'NYONT'                                                         
         DC    C'OCCAT'                                                         
         DC    C'PISTT'                                                         
         DC    C'PREST'                                                         
         DC    C'RAD T'                                                         
         DC    C'REWTT'                                                         
         DC    C'RHAST'                                                         
         DC    C'RHUAT'                                                         
         DC    C'SANTT'                                                         
         DC    C'SDCRT'                                                         
         DC    C'SNCST'                                                         
         DC    C'SPCHT'                                                         
         DC    C'SPCNT'                                                         
         DC    C'TALAT'                                                         
         DC    C'TBQPT'                                                         
         DC    C'TBS T'                                                         
         DC    C'TCI T'                                                         
         DC    C'TEART'                                                         
         DC    C'TFGXT'                                                         
         DC    C'THOMT'                                                         
         DC    C'TITVT'                                                         
         DC    C'TJTCT'                                                         
         DC    C'TKRGT'                                                         
         DC    C'TPMIT'                                                         
         DC    C'UNIVT'                                                         
         DC    C'VCBLT'                                                         
         DC    C'XEPMT'                                                         
         DC    C'XERVT'                                                         
         DC    C'XETVT'                                                         
         DC    C'XEWTT'                                                         
         DC    C'XHAST'                                                         
         DC    C'XHBCT'                                                         
         DC    C'XHBJT'                                                         
         DC    C'XHBMT'                                                         
         DC    C'XHBRT'                                                         
         DC    C'XHFTT'                                                         
         DC    C'XHFXT'                                                         
         DC    C'XHLRT'                                                         
         DC    C'XHUAT'                                                         
         DC    C'XUPNT'                                                         
         DC    C'ZCSNT'                                                         
         DC    X'00'                                                            
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
*UB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
ELCODE   DS    X                                                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         EJECT                                                                  
DARERECD DSECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
NBUYRECD DSECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
TRFPATD  DSECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007STLDEXTWHO08/18/00'                                      
         END                                                                    
