*          DATA SET CTRDCTID   AT LEVEL 080 AS OF 08/10/00                      
*PHASE RDCNTIDA                                                                 
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE ADSCAN                                                                 
*                                                                               
         TITLE 'CTRDCTID - SCAN AND PRINT OUT RECORDS - SPARTAN'                
***********************************************************************         
*                                                                     *         
*  PURPOSE: READ ACCESS AND ID RECS FROM CTFILE  AND PRINT OUT KEY    *         
*                                                                     *         
***********************************************************************         
*                                                                               
RDCTID   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*RDCTID*,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',FLIST,IO                 
         MVI   DATADISP+1,28                                                    
*                                                                               
* BUILD A TABLE OF NECESSARY INFO FROM THE ACCESS RECORDS                       
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R7,IO                                                            
         USING CT5REC,R7           ACCESS RECORDS                               
         MVI   KEY,CT5KTYPQ                                                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
         B     BASE20                                                           
*                                                                               
BASE10   DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'CTFILE',KEY,IO                    
         TM    DMCB+8,X'80'                                                     
         BNZ   MX00                                                             
*                                                                               
BASE20   CLI   CT5KTYP,CT5KTYPQ    IS THIS AN ACCESS RECORD?                    
         BH    MX00                NO MORE -> LOOK FOR ID RECX                  
         BL    BASE10              NOPE - GET NEXT                              
*                                                                               
         LA    R5,TABLE                                                         
BASE30   OC    0(4,R5),0(R5)       SPACE TAKEN                                  
         BZ    BASE35                                                           
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   BASE30                                                           
         DC    H'0'                                                             
*                                                                               
BASE35   MVC   0(2,R5),CT5KALPH                                                 
         LR    R6,R7                                                            
         MVI   ELCODE,CTSYSELQ                                                  
         BAS   RE,GETEL                                                         
         BE    BASE40                                                           
*                                                                               
*        MVC   P+1(27),=C'ACCESS REC WITH NO 21 ELEM:'                          
*        GOTO1 =V(HEXOUT),DMCB,(R7),P+30,52,=C'TOG'                             
*        GOTO1 =V(PRINTER)                                                      
         B     BASE10                                                           
*                                                                               
BASE40   DS    0H                                                               
         USING CTSYSD,R6                                                        
         MVC   2(1,R5),CTSYSAGB                                                 
         MVC   3(1,R5),CTSYSSE                                                  
         DROP  R6,R7                                                            
*                                                                               
         B     BASE10              GET NEXT ONE                                 
*                                                                               
* SEQUENTIALLY READ ALL ID RECS IN CTFILE AND MAKE THE PRINTLINE                
*                                                                               
MX00     XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R6,IO                                                            
         USING CTIREC,R6                                                        
         MVI   KEY,CTIKTYPQ                                                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
MX10     DS    0H                                                               
         LA    R6,IO                                                            
         USING CTIREC,R6                                                        
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ    IS THIS AN ID REC?                           
         BNE   MX100               NOT YET -> GET NEXT                          
         OC    CTIKID(8),CTIKID    PASSIVE KEY?                                 
         BZ    MX100                YES                                         
*                                                                               
* PUT INFO INTO PRINTLINE                                                       
*                                                                               
         LA    R3,PANCARD                                                       
         USING PCARDD,R3                                                        
*                                                                               
         MVC   PUSERID,CTIKID      USER ID                                      
*                                                                               
         DROP  R6                                                               
         MVI   ELCODE,CTAGYELQ     AGY ID ELEM                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTAGYD,R6                                                        
*                                                                               
         MVC   PALPHID,CTAGYID     ALPHA ID                                     
         DROP  R6                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,CTDSTELQ     DESTINATION ELEM                             
         BAS   RE,GETEL                                                         
         BNE   MX30                NO DESTINATION ELEM                          
         USING CTDSTD,R6                                                        
         MVC   PDEPTNM,CTDSTNAM    DESTINATION NAME                             
         MVC   PDEPTADD,CTDSTADD   ADDRESS LINE ONE                             
*                                                                               
         GOTO1 =V(ADSCAN),DMCB,(33,CTDSTAD2),(20,PDEPCITY),PDEPSTA,    +        
               (5,PDEPZIP)                                                      
         CLI   DMCB+3,0                                                         
         BE    MX50                                                             
*                                                                               
MX30     MVC   PDEPCITY,STARS                                                   
         MVC   PDEPSTA,STARS       STARS MARK INVALID ADDRESS INPUT             
         MVC   PDEPZIP,STARS                                                    
*                                                                               
         CLI   0(R6),CTDSTELQ                                                   
         BE    MX50                                                             
         MVC   PATTN,STARS                                                      
         MVC   PPOWER,STARS                                                     
         MVC   PDEPTNM,STARS                                                    
         MVC   PDEPTADD,STARS                                                   
         B     MX70                                                             
*                                                                               
MX50     MVC   PATTN,CTDSTAD3                                                   
         MVC   PPOWER,CTDSTPOW     POWER CODE                                   
         DROP  R6                                                               
*                                                                               
MX70     DS    0H                                                               
         LA    R5,TABLE            SEARCH TABLE FOR CORRESPONDING INFO          
MX75     CLC   PALPHID,0(R5)                                                    
         BE    MX78                                                             
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   MX75                                                             
         DC    H'0'                                                             
*                                                                               
MX78     LA    R5,2(R5)            AGENCY BINARY NUMBER                         
         GOTO1 =V(HEXOUT),DMCB,(R5),PSEAGY#,1,=C'TOG'                           
         LA    R5,1(R5)            SE NUMBER                                    
*                                                                               
         L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
         CLC   SESYS,0(R5)                                                      
         BE    MX80                                                             
         BXLE  R1,RE,*-10                                                       
*                                                                               
MX80     MVC   PSENAME,SENAME                                                   
         MVI   PCOM1,COMMA                                                      
         MVI   PCOM2,COMMA                                                      
         MVI   PCOM3,COMMA                                                      
         MVI   PCOM4,COMMA                                                      
         MVI   PCOM5,COMMA                                                      
         MVI   PCOM6,COMMA                                                      
         MVI   PCOM7,COMMA                                                      
         MVI   PCOM8,COMMA                                                      
         MVI   PCOM9,COMMA                                                      
         MVI   PSLASH,SLASH                                                     
*                                                                               
         PUT    FILEOUT,(R3)                                                    
                                                                                
MX100    DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'CTFILE',KEY,IO                    
         TM    DMCB+8,X'80'                                                     
         BZ    MX10                                                             
MXB      CLOSE FILEOUT                                                          
         XBASE                                                                  
         XIT1                                                                   
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RDCTID,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         DC    C'**KEY***'                                                      
KEY      DS    XL25                BTAM KEY                                     
KEYSAVE  DS    XL25                USED TO INSURE EXCLUSIVE KEYS                
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL255                                                            
*                                                                               
PANCARD  DS    CL160                                                            
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=PM,RECFM=FB               *        
               LRECL=160,BLKSIZE=8000                                           
*                                                                               
STARS    DC    33C'*'                                                           
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
*                                                                               
TABLE    DS    XL3000                                                           
         DC    X'FF'                                                            
*                                                                               
PCARDD   DSECT                                                                  
PUSERID  DS    CL8                                                              
PCOM1    DS    C                                                                
PALPHID  DS    CL2                                                              
PCOM2    DS    C                                                                
PSENAME  DS    CL4                                                              
PSLASH   DS    C                                                                
PSEAGY#  DS    CL2                                                              
PCOM3    DS    C                                                                
PDEPTNM  DS    CL33                                                             
PCOM4    DS    C                                                                
PDEPTADD DS    CL33                                                             
PCOM5    DS    C                                                                
PDEPCITY DS    CL20                                                             
PCOM6    DS    C                                                                
PDEPSTA  DS    CL2                                                              
PCOM7    DS    C                                                                
PDEPZIP  DS    CL5                                                              
PCOM8    DS    C                                                                
PATTN    DS    CL33                                                             
PCOM9    DS    C                                                                
PPOWER   DS    CL4                                                              
*                                                                               
SLASH    EQU   C'/'                                                             
COMMA    EQU   C','                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080CTRDCTID  08/10/00'                                      
         END                                                                    
