*          DATA SET DMDATESTLA AT LEVEL 051 AS OF 05/01/02                      
*          DATA SET DMDATEST   AT LEVEL 061 AS OF 08/03/99                      
*PHASE DMDATSTL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*                                                                               
         TITLE 'DMDATEST - TEST NEW BLKIND LINKS'                               
*                                                                               
KEYLEN   EQU   17                                                               
*                                                                               
DMDATSTL CSECT                                                                  
         ENTRY ADWAIT                                                           
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         PRINT NOGEN                                                            
         NBASE WORKL,DMDATSTL,=V(REGSAVE),RA,CLEAR=YES                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         BRAS  RE,INIT                                                          
*                                                                               
         LA    R8,DAFILE                                                        
         USING DTFPHD,R8                                                        
*                                                                               
         OC    DSKADR,START        START D/A SET?                               
         BNZ   LOOP03              NO                                           
         MVC   DSKADR,=X'00010100'                                              
         B     LOOP03                                                           
*                                                                               
TRKNXT   LH    RF,TRACK            NEXT TRACK                                   
         LA    RF,1(RF)                                                         
         STH   RF,TRACK                                                         
         XC    BLOCK,BLOCK                                                      
*                                                                               
BLKNXT   LH    RF,BLOCK            NEXT BLOCK                                   
         AHI   RF,X'0100'                                                       
         STH   RF,BLOCK                                                         
*                                                                               
LOOP03   MVC   P6,DSKADR                                                        
         OC    END,END             END D/A SET?                                 
         BZ    LOOP03A             NO                                           
         CLC   DSKADR,END                                                       
         BH    CLOSE                                                            
*                                                                               
LOOP03A  GOTO1 VDADDS,P1,01,AIOA,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    BL02                                                             
         TM    P3+1,X'08'          BLOCK NOT FOUND                              
         BO    TRKNXT                                                           
         TM    P3+1,X'04'          EOF                                          
         BO    CLOSE                                                            
         DCHO                                                                   
*                                                                               
*        GOTO1 VHEXOUT,DMCB,P6,P+1,4                                            
*        MVC   P+10(8),=CL8'READ'                                               
*        GOTO1 VPRINTER                                                         
*                                                                               
BL02     CLC   MODE,BADLINK        LOOKING FOR A BAD LINK D/A                   
         BNE   BL04                NO                                           
         L     RF,AIOA                                                          
         CLC   LINK,2(RF)          MATCH BAD D/A REQUESTED?                     
         BNE   BLKNXT              NO                                           
         BRAS  RE,PRTBUFF                                                       
         B     BLKNXT                                                           
*                                                                               
BL04     CLC   MODE,DUMPALL        PRINTING OUT ALL BLOCKS?                     
         BNE   BL06                NO                                           
         BRAS  RE,PRTBUFF                                                       
         B     BLKNXT                                                           
*                                                                               
BL06     CLC   MODE,BLKIND         PRINTING OUT BLKIND CHAINS?                  
         BNE   BL08                NO                                           
         BRAS  RE,DBLKIND                                                       
         B     BLKNXT                                                           
*                                                                               
BL08     CLC   MODE,CHAIN          PRINTING OUT BLKIND CHAINS?                  
         BNE   BL10                NO                                           
         BRAS  RE,DCHAIN                                                        
         B     BLKNXT                                                           
*                                                                               
BL10     DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FOLLOW (AND PRINT) ANY CHAINED INDEX BLOCKS              *         
***********************************************************************         
         SPACE 1                                                                
DCHAIN   NTR1  ,                                                                
         L     R3,AIOA                                                          
         OC    2(4,R3),2(R3)       CHAIN HERE?                                  
         BZ    EXITOK                                                           
*                                                                               
         MVC   P(30),=CL30'SPLIT BLOCK STARTS HERE'                             
         GOTO1 VPRINTER                                                         
         BRAS  RE,PRTBUFF                                                       
*                                                                               
LOOP08   MVC   P6,2(R3)            SAVE SEQUENTIAL READ CHAIN                   
         GOTO1 VDADDS,P1,01,AIOA,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    LOOP10                                                           
         DCHO                                                                   
*                                                                               
LOOP10   MVC   P(30),=CL30'FOLLOWING CHAIN'                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R3,AIOA                                                          
         BRAS  RE,PRTBUFF                                                       
         OC    2(4,R3),2(R3)                                                    
         BNZ   LOOP08                                                           
*                                                                               
         MVC   P(30),=CL30'END OF CHAIN'                                        
         GOTO1 VPRINTER                                                         
         B     CLOSE                                                            
*&&                                                                             
****                                                                            
         SPACE 1                                                                
*                                                                               
CLOSE    GOTO1 VDADDS,P1,15,,,DAFILE     CLOSE THE FILE                         
         MVC   P(20),=CL20'CLOSE'                                               
         GOTO1 VPRINTER                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FOLLOW BLKIND LINKS TO ENSURE FILE INTEGRITY             *         
***********************************************************************         
         SPACE 1                                                                
DBLKIND  NTR1  ,                                                                
         L     R3,AIOA                                                          
         CLC   BLKIND,2(R3)        IS THIS A BLKIND BLOCK?                      
         BNE   EXITOK              NO                                           
*                                                                               
         MVI   BADFLAG,C'N'                                                     
         MVC   P(30),=CL30'*** FOUND BLKIND INDEX BLOCK'                        
         GOTO1 VPRINTER                                                         
         BRAS  RE,PRTBUFF                                                       
*                                                                               
         AHI   R3,6                POINT TO FIRST D/A IN LIST                   
         MVC   CHAIN,0(R3)         SAVE IT FOR SEQUENTIAL READ CHAIN            
*                                                                               
         MVC   P(30),=CL30'*** ANALYSING INDEX ENTRIES'                         
         GOTO1 VPRINTER                                                         
*                                                                               
BIN02    MVC   P6,0(R3)                                                         
         GOTO1 VDADDS,P1,01,AIO2,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,PRTBUFF2                                                      
         AHI   R3,KEYLEN                                                        
         OC    0(4,R3),0(R3)       END OF INDEX BLOCK?                          
         BNZ   BIN02               NO                                           
*                                                                               
         MVC   P(30),=CL30'*** END OF INDEX ENTRIES'                            
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   BADFLAG,C'Y'                                                     
         BNE   EXITOK                                                           
         MVC   P(30),=CL30'*** INVALID INDEX ENTRY'                             
         GOTO1 VPRINTER                                                         
         MVC   P(30),=CL30'    FOLLOWING CHAIN'                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   BADFLAG,C'C'        SET CHAIN FOLLOWING MODE                     
         MVC   P6,CHAIN                                                         
BIN04    GOTO1 VDADDS,P1,01,AIO2,0,,P6                                          
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,PRTBUFF2                                                      
         L     RE,AIO2                                                          
         OC    2(4,RE),2(RE)                                                    
         BZ    BIN06                                                            
         MVC   P6,2(RE)                                                         
         B     BIN04                                                            
*                                                                               
BIN06    MVC   P(30),=CL30'*** END OF CHAIN'                                    
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
***********************************************************************         
*        INITIALISATION ROUTINE                                       *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'CTITLE),CTITLE   SET UP TITLE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(L'PTITLE),PTITLE                                               
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'2'                                                   
         MVC   P(L'PTITLEU),PTITLEU                                             
         BASR  RE,RF                                                            
         MVI   SPACING+3,C'1'                                                   
*                                                                               
INIT02   GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END OF CARDS?                                
         BE    INIT04              YES                                          
         MVC   P(L'CARD),CARD                                                   
         GOTO1 VPRINTER            PRINT PARAMETER CARD                         
*                                                                               
         LA    R1,CARD             VALIDATE KEYWORD=VALUE                       
         BRAS  RE,CARDVAL                                                       
         BE    INIT02                                                           
         B     XBASE               CC:NE MEANS INVALID KEYWORD                  
*                                                                               
INIT04   GOTO1 VPRINTER            PRINT SPACE LINE                             
         GOTO1 VDADDS,P1,14,,,DAFILE                                            
         MVC   P(L'FOPEN),FOPEN                                                 
         GOTO1 VPRINTER            OUTPUT FILE OPEN MESSAGE                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT CONTENTS OF BUFFER                                 *         
* NTRY: P2 B0-3 = A(BUFFER)                                           *         
*       P3 B2-3 = L'BUFFER                                            *         
*       P6 B0-3 = D/A OF BUFFER                                       *         
***********************************************************************         
         SPACE 1                                                                
PRTBUFF  NTR1  ,                                                                
         GOTO1 VPRNTBL,DMCB,=C'D/A',P6,C'DUMP',4,=C'1D'                         
         LH    R0,P3+2                                                          
         GOTO1 VPRNTBL,DMCB,=C'IOA',AIOA,C'DUMP',(R0),=C'1D'                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SPECIAL PRINT ROUTINE FOR BUFFER 2 FOR BLKLIND CHAIN FOLLOWING      *         
* NTRY: R3 =   A(BLKIND INDEX ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTBUFF2 NTR1  ,                                                                
         MVC   P(4),=CL4'D/A='                                                  
         GOTO1 VHEXOUT,DMCB,P6,P+4,4                                            
*                                                                               
         MVC   P+15(15),=CL15'FOLLOWING CHAIN'                                  
         CLI   BADFLAG,C'C'                                                     
         BE    PB202                                                            
*                                                                               
         MVC   P+15(15),=CL15'LINK IS GOOD'                                     
         L     R2,AIO2                                                          
         CLC   2(4,R2),KEYLEN(R3)                                               
         BE    PB202                                                            
*                                                                               
         MVI   BADFLAG,C'Y'                                                     
         MVC   P+15(15),=CL15'**LINK IS BAD**'                                  
*                                                                               
PB202    AH    R2,P3+2                                                          
         AHI   R2,-13                                                           
         MVC   P+31(6),=CL8'HIKEY='                                             
         GOTO1 VHEXOUT,DMCB,(R2),P+37,13                                        
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ADWAIT                                                              *         
***********************************************************************         
         SPACE 1                                                                
ADWAIT   BR    RE                                                               
         USING *,RF                                                             
         LTR   RE,RE               RE IS NEG IF 31 BIT CALLER                   
         BP    ADWAITX                                                          
         AP    IOCOUNT,PCKD1                                                    
ADWAITX  BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',(R2)),(1,SCNBLK)                             
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CARDV10  CLI   CTYPE,CTHEX         HEX INPUT                                    
         BNE   CARDV12             NO                                           
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
*                                                                               
         MVI   WORK,C'0'           ZERO FILL TEMP AREA                          
         MVC   WORK+1(L'WORK-1),WORK                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,CLEN                                                        
         SLL   RF,1                LENGTH OF OUTPUT AREA AS EBCDIC              
         LR    R1,RF                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,SC2NDLEN                                                    
         SR    R1,RE               R1=DISPLACEMENT INTO WORK FOR MOVE           
         LA    R1,WORK(R1)                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),SC2NDFLD    RIGHT ALIGN INPUT DATA                       
*                                                                               
         ICM   R0,15,COUT                                                       
         GOTO1 VHEXIN,DMCB,WORK,(R0),(RF)                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   EXITOK                                                           
         B     CENOTHEX                                                         
*                                                                               
CARDV12  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,=CL40'Invalid line format'                                    
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,=CL40'Invalid Keyword'                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,=CL40'Value not a valid number'                               
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,=CL40'Value not a valid character string'                     
         B     CERR                                                             
*                                                                               
CENOTHEX LA    R1,=CL40'Value not a valid hex string'                           
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,=CL40'Length of input string too short'                       
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,=CL40'Length of input string too long'                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,=CL40'Numeric value too small'                                
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,=CL40'Numeric value too large'                                
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,=CL40'Invalid/missing value'                                  
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   P,SPACES                                                         
         MVC   P(15),=CL15' *** ERROR ***'                                      
         MVC   P+15(40),0(R1)                                                   
         GOTO1 VPRINTER                                                         
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         ORG   DMDATSTL+((((*-DMDATSTL)/16)+1)*16)                              
         SPACE 1                                                                
CARDTAB  DC    CL8'MODE    ',F'001',F'010'                                      
         DC    AL1(03,CTCHR,L'MODE,0),AL4(MODE)                                 
         DC    CL8'BADLINK ',F'008',F'008'                                      
         DC    AL1(04,CTHEX,L'LINK,0),AL4(LINK)                                 
         DC    CL8'START   ',F'008',F'008'                                      
         DC    AL1(02,CTHEX,L'STRT,0),AL4(STRT)                                 
         DC    CL8'END     ',F'008',F'008'                                      
         DC    AL1(06,CTHEX,L'END,0),AL4(END)                                   
***                                                                             
         DC    CL8'KILL    ',F'001',F'0000004'                                  
         DC    AL1(03,CTCHR,L'KILL,0),AL4(KILL)                                 
         DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'SYSTEM  ',F'001',F'0000004'                                  
         DC    AL1(05,CTCHR,L'SYSTEM,0),AL4(SYSTEM)                             
         DC    CL8'Y2K     ',F'001',F'0000001'                                  
         DC    AL1(02,CTCHR,L'DCY2K,0),AL4(DCY2K)                               
         DC    CL8'UTL     ',F'001',F'0050000'                                  
         DC    AL1(02,CTNUM,0,0),AL4(DCUTL)                                     
         DC    CL8'TBUFF   ',F'001',F'0005000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCTBUFF)                                   
         DC    CL8'TPRNT   ',F'001',F'0050000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCTPRNT)                                   
         DC    CL8'LOCKET  ',F'001',F'0050000'                                  
         DC    AL1(02,CTNUM,0,0),AL4(DCLOCK)                                    
         DC    CL8'JOBTAB  ',F'001',F'0005000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCJOB)                                     
         DC    CL8'IAMTAB  ',F'001',F'0005000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCIAM)                                     
         DC    CL8'TSTTAB  ',F'002',F'0000020'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCTST)                                     
         DC    CL8'DARETAB ',F'000',F'0005000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDARE)                                    
         DC    CL8'ASSIST  ',F'000',F'0008000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCASS)                                     
         DC    CL8'TSTTRKS ',F'001',F'0000020'                                  
         DC    AL1(06,CTNUM,0,0),AL4(PTSTDA)                                    
         DC    CL8'VRSNTAB ',F'001',F'0005000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCVRSN)                                    
         DC    CL8'BCTAB   ',F'001',F'0005000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCBRD)                                     
         DC    CL8'TRCBUFF ',F'000',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(STRC)                                      
         DC    CL8'SCTBUFF ',F'000',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(SSCT)                                      
         DC    CL8'TEMPEST ',F'001',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(TPST)                                      
         DC    CL8'REPDUMPS',F'001',F'0000050'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPR)                                    
         DC    CL8'ADVDUMPS',F'001',F'0000050'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPA)                                    
         DC    CL8'TSTDUMPS',F'001',F'0000100'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPT)                                    
         DC    CL8'LOCKTAB ',F'001',F'1000000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(NLOCKS)                                    
         DC    CL8'PROFILE1',F'001',F'1000000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCUSR1)                                    
         DC    CL8'PROFILE2',F'001',F'1000000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCUSR2)                                    
         DC    CL8'PROFILE3',F'001',F'1000000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCUSR3)                                    
         DC    CL8'CTIRECS ',F'001',F'0050000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCIDS)                                     
         DC    CL8'DDISP   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDDSP)                                    
         DC    CL8'DBOOK   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDBOK)                                    
         DC    CL8'DSTATION',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDSTN)                                    
         DC    CL8'DMASTER ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDMST)                                    
         DC    CL8'DNAME   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDNAM)                                    
         DC    CL8'DCODE   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDCDE)                                    
         DC    CL8'DCONTROL',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDCTL)                                    
         DC    CL8'DADJUST ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDADJ)                                    
         DC    CL8'DFORMULA',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDFRM)                                    
         DC    CL8'DFMTAB  ',F'001',F'0001000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCDFMTB)                                   
         DC    CL8'ALPHMKT ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDALF)                                    
         DC    CL8'NADUNV  ',F'001',F'0001000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCDNUN)                                    
*                                                                               
CARDTABX DC    AL1(CARDEOT)                                                     
         SPACE 2                                                                
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CTHEX    EQU   3                   8 CHARACTER HEX                              
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
DMDATSTL CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
CTITLE   DC    CL60'D/A READ OF I/S FILE'                                       
PTITLE   DC    CL60'INPUT PARAMETER CARDS'                                      
PTITLEU  DC    CL60'---------------------'                                      
FOPEN    DC    CL20'FILE HAS BEEN OPENED'                                       
FCLOSE   DC    CL20'FILE HAS BEEN CLOSED'                                       
*                                                                               
IOCOUNT  DC    PL4'0'                                                           
PCKD1    DC    PL1'1'                                                           
         DS    0D                                                               
BLKIND   DC    XL4'FFFFFFFF'                                                    
VCPRINT  DC    V(CPRINT)                                                        
VCARDS   DC    V(CARDS)                                                         
VDADDS   DC    V(DADDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VSCANNER DC    V(SCANNER)                                                       
*                                                                               
MODE     DC    CL8'        '                                                    
BADLINK  DC    CL8'BADLINK '                                                    
DUMPALL  DC    CL8'DUMPALL '                                                    
BLOCKS   DC    CL8'BLKIND  '                                                    
CHAIN    DC    CL8'BLKIND  '                                                    
LINK     DC    F'0'                                                             
STRT     DC    F'0'                                                             
END      DC    F'0'                                                             
         EJECT                                                                  
         DS    0D                                                               
AIOA     DC    A(IOA)                                                           
AIO2     DC    A(IO2)                                                           
*                                                                               
WORK     DC    20F'0'                                                           
CARD     DS    CL80                                                             
SVC      DS    CL80                                                             
MVSDUMP  DC    C'N'                                                             
KEYST    DC    X'00'                                                            
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
UTL      DC    255X'00'                                                         
*                                                                               
SSB      DC    X'0000',X'FF',XL252'00'                                          
         PRINT GEN                                                              
DAFILE   DMDA  BLKSIZE=4628                                                     
D2FILE   DMDA  BLKSIZE=4628                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'**IOA**'                                                     
IOA      DS    16000C                                                           
         DS    0D                                                               
         DC    CL8'**IO2**'                                                     
IO2      DS    16000C                                                           
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
COUNT    DS    PL4                                                              
MAXCOUNT DS    PL4                                                              
BADFLAG  DS    C                                                                
CHAIN    DS    F                                                                
DMCB     DS    6F                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
DSKADR   DS    0F                                                               
TRACK    DS    H                                                                
BLOCK    DS    H                                                                
WORK     DS    XL80                                                             
SCNBLK   DS    3CL(SCBLKLQ)                                                     
WORKL    EQU   *-WORKD                                                          
*                                                                               
         DROP  R8,R9,RA,RB                                                      
         EJECT                                                                  
***********************************************************************         
* VARIABLE SCAN MODULE                                                *         
***********************************************************************         
         SPACE 1                                                                
SCANNER  CSECT                                                                  
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         SR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
         SPACE 2                                                                
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
         SPACE 2                                                                
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BAS   RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
         SPACE 2                                                                
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
         SPACE 2                                                                
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
         SPACE 2                                                                
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
         SPACE 2                                                                
VARPAK   PACK  SDUB,0(0,R2)                                                     
         SPACE 2                                                                
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
         SPACE 2                                                                
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
         SPACE 2                                                                
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
         SPACE 2                                                                
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
         SPACE 2                                                                
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
         SPACE 2                                                                
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
         SPACE 2                                                                
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
         SPACE 2                                                                
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
         SPACE 2                                                                
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
         SPACE 2                                                                
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
         SPACE 2                                                                
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
         SPACE 2                                                                
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
         SPACE 2                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
SWORKL   EQU   *-SWORKD                                                         
         SPACE 1                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051DMDATESTLA05/01/02'                                      
         END                                                                    
